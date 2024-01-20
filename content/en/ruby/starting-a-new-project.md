---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Leading a new project is like paving a fresh path. You chart your course, set the tone and tackle the first steps together with your team. Programmers embark on new projects to solve problems, explore ideas, innovate, or simply to learn and grow.

## How to:

A Ruby project starts with a blank directory. It all takes off from there. Let's assume we are creating a new Ruby project named "demo". To create a new Ruby project:

```Ruby
mkdir demo
cd demo 
```

After this, you set up your Gemfile. A Gemfile outlines all dependencies for your Ruby application. Let's make one:

```Ruby
# Creates a Gemfile inside demo directory
touch Gemfile
```

You can now add any gems you need. Let's say we need 'rspec' for testing, your Gemfile would look something like this:

```Ruby
source "https://rubygems.org"

gem 'rspec'
```

Don't miss running `bundle install` to install the gems specified in your Gemfile.

```Ruby
bundle install
```

After you've got your Gemfile sorted, you might want to setup a lib directory for your Ruby files:

```Ruby
mkdir lib
```

And that's it! You've got a basic Ruby project set up.

## Deep Dive

Historically, Ruby's initial release was in the mid-1990s, its simplicity, readability, and flexibility have remained its core principles. Today, Ruby has a wide array of libraries (gems) for virtually any programming task, thereby making starting new projects easier and more efficient. 

Alternative ways of starting a new Ruby project involve using frameworks like Rails, Sinatra, or Hanami, which provide their own project structure and tooling. These alternatives could enhance productivity, but it's great to know how the underlying processes work.

When creating a new project, certain details like managing dependencies with Bundler, version control with Git, and testing environments are crucial. Developers often use a .gitignore file to exclude files from Git that should stay local, like secrets or logs.

## See Also

- [The official Ruby website](https://www.ruby-lang.org/)
- [RSpec testing for Ruby](https://rspec.info/)
- [Learning Ruby from the ground up](https://www.codecademy.com/learn/learn-ruby)
- [Getting started with Rails](https://guides.rubyonrails.org/getting_started.html)
- [Ruby project structure best practices](https://guides.rubyonrails.org/initialization.html)