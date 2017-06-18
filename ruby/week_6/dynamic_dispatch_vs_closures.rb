class A
  def even x
    puts "in even A"
    if x==0 then true else even(x-1) end
  end

  def odd x
    puts "in odd A"
    if x==0 then false else even(x-1) end
  end
end

a1 = A.new.odd 7
puts "a1 is " + a1.to_s + "\n\n"

class B < A
  def even x
    puts "in even B"
    x % 2 == 0
  end
end

class C < A
  def even x
    puts "in even c"
    false
  end
end

a2 = B.new.odd 7
puts "a2 is " + a2.to_s + "\n\n"

a3 = C.new.odd 7
puts "a3 is " + a3.to_s + "\n\n"
