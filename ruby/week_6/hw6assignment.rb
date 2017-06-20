# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [
    [[[0, 0], [1, 0], [0, 1], [1, 1]]],
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
    [[[0, 0], [-1, 0], [1, 0], [2, 0]],
     [[0, 0], [0, -1], [0, 1], [0, 2]],],
    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),

    rotations([[0, 0], [0, -1], [1, -1], [1, 0], [1, 1]]),
    [[[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]],
     [[0, 0], [0, -1], [0, 1], [0, 2], [0, 3]]],
    rotations([[0, -1], [1, -1], [1, 0]])]

  Cheat_Piece = [[[[0, 0]]]]
  # your enhancements here

    def initialize(point_array, board)
      super(point_array, board)
      @cheat = true
    end

    def self.next_piece(board, cheat=false)
      if cheat
         MyPiece.new(Cheat_Piece[0], board)
      else
        MyPiece.new(All_My_Pieces.sample, board)
      end
    end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end
   
  #rotate 180 degrees
  def rotate_180_degrees
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  def cheat
    if !game_over? and @game.is_running?
      @cheat = true
    end
    draw
  end

  def next_piece
    if @cheat
      if @score >= 100
        @score = @score - 100
      end
      @current_block = MyPiece.next_piece(self, @cheat)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    my_key_bindings
    buttons
    run_game
  end

    def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoard.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw
    end

    def my_key_bindings
      key_bindings
      @root.bind('u', proc {@board.rotate_180_degrees})
      @root.bind('c', proc {@board.cheat})
    end
end


