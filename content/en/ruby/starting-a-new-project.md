---
title:                "Ruby recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why

Why would someone want to start a new programming project in Ruby? Well, Ruby is a popular and versatile programming language that is known for its simplicity and expressiveness. So, if you want to create a project that is both efficient and easy to read, Ruby is a great choice.

## How To

To start a new project in Ruby, you will need to follow a few steps. First, make sure you have Ruby installed on your computer. You can check by typing ```ruby --version``` into your command line. If you don't have Ruby installed, you can download it from the official website.

After installing Ruby, you can use a gem called Bundler to manage your project's dependencies. Simply type ```gem install bundler``` into your command line. Then, create a new folder for your project and navigate to it in your command line.

Next, use Bundler to create a new project by typing ```bundle gem project_name```, replacing "project_name" with the name of your project. This will create a basic project structure for you.

Now, you can start writing your code in the "lib" folder and running it by using the ```ruby``` command followed by the name of your file.

```Ruby
# Sample code
def add_numbers(num1, num2)
  return num1 + num2
end

puts add_numbers(5, 7)
# Output: 12
```

## Deep Dive

Starting a new project in Ruby also means familiarizing yourself with some key concepts and tools. For example, understanding how to use Bundler to manage your project's dependencies is crucial. You'll also want to explore Ruby's object-oriented programming features, such as classes and modules, to create well-structured and reusable code.

Furthermore, learning about testing in Ruby is important in order to ensure the quality and functionality of your project. Tools like RSpec and MiniTest can help you write tests and catch any bugs or errors in your code.

There are also various helpful resources available online, such as documentation, forums, and online communities, that can assist you in your journey of starting a new project in Ruby.

## See Also

- [Official Ruby Website](https://www.ruby-lang.org/en/)
- [Bundler Documentation](https://bundler.io/)
- [RSpec Documentation](https://rspec.info/)
- [MiniTest Documentation](https://github.com/seattlerb/minitest)
- [Ruby-Doc.org - Ruby Documentation and Forums](https://ruby-doc.org/)