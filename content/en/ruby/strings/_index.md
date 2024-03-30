---
type: page_chapter
---

In my opinion, Ruby's strings are pretty much the height of working with text. 99% of what I want to do with strings turns about to be a built-in method.

It's effortless to apply regular expressions, split and manipulate strings. Even to move back and forth between strings and arrays:

```ruby
puts "Hello, Ruby World!"
  .capitalize
  .gsub(/w\w+/, 'programmers')
  .split
  .first
  .include?('H')

# => true
```

Very few programming languages enable a fluent working style like that.    
— [Robb](https://forkful.com/en/about)
