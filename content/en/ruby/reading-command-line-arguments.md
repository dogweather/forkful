---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

---

## What & Why?
Reading command line arguments in Ruby is all about taking user-defined data in the form of arguments directly from the command line. This enables scripts to have dynamic functionality based on those inputs.

## How-to
Below is a simple Ruby script to read command line arguments:

```Ruby
ARGV.each do |arg|
  puts "Argument: #{arg}"
end
```
Just enter some arguments after your script name, like
```
ruby your_file.rb arg1 arg2 arg3
```
and watch your program spit out:

```
Argument: arg1
Argument: arg2
Argument: arg3
```

## Deep Dive

Historically, command-line arguments trace back to Unix and Linux systems where interactive shell scripting thrived. It was later adopted in programming languages as a common feature to accept user inputs.

Instead of `ARGV`, you can use the `OptionParser` library in Ruby for more complex command line parsing where options are specified with `-` or `--`.

Behind the scenes of reading command line arguments, Ruby stores arguments in an array `ARGV`, with indexing starting at zero.

```Ruby
# To access the first argument 
puts ARGV[0]

# To access the second argument
puts ARGV[1]
```

When no arguments are provided, accessing ARGV[index] will return nil.

Remember, ARGV is case-sensitive and it's a constant, so you can't assign another array to it.

## See Also

1. [Ruby Documentation on ARGV](https://docs.ruby-lang.org/en/3.0.0/ARGF.html)
2. [Ruby OptionParser Library](https://ruby-doc.org/stdlib-2.5.1/libdoc/optparse/rdoc/OptionParser.html)
3. [Command Line Basics](http://www.linfo.org/command_line.html)
4. [Unix Shell Scripting](http://www.freeos.com/guides/lsst/index.html)