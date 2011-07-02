PERFTEST = "Performance.hs"
COUNT = 100

def doctest(count) 
  """
-- | #{count}
--
-- >>> putStrLn \"test #{count}\" 
-- test #{count}
--

d#{count} :: ()
d#{count} = ()

"""

end

File.open(PERFTEST, "w+") do |fh|
  fh.write"""module Performance where

"""
  COUNT.times do |i| 
    fh.write doctest(i)
  end
end
