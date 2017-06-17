class Point
  attr_accessor :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end
  def distFromOrigin
    Math.sqrt(@x * @x + @y * @y)
  end
  def distFromOrigin2
    Math.sqrt(x * x + y * y)
  end

end

class ColorPoint < Point
  attr_accessor :color

  def initialize(x,y,c="clear")
    super(x,y)
    @color = c
  end
end
