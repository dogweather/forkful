---
title:                "Starting a new project"
date:                  2024-01-20T18:04:12.214704-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is like planting a seed in your digital garden—you're kicking off a fresh bundle of ideas, and turning them into code that does something useful. Programmers start new projects to solve problems, explore concepts, or just for the pure joy of creating something new.

## How to:

So, you're ready to turn those brainwaves into a Ruby project? Let's roll. Begin with the basics.

```Ruby
# Install Ruby, if you haven't already.
# Check your Ruby version to make sure you're up to date:
$ ruby -v

# Output should be Ruby's current version:
# ruby 3.x.x

# Next, let's create a directory for your project:
$ mkdir my_new_project
$ cd my_new_project

# Now, initiate a new Git repository if you want version control (highly recommended):
$ git init

# Then create an entry file, let's call it 'app.rb':
$ touch app.rb

# Start coding! Write a simple output to ensure it's working:
puts "Hello New Project!"

# Run your file:
$ ruby app.rb

# Output should be:
# Hello New Project!
```

## Deep Dive

Back in the day, starting a new Ruby project was a bit raw—just you, a text editor, and a bunch of `.rb` files. As the language has grown, tools have popped up to streamline this process.

For instance, Bundler manages your gems—Ruby libraries—so you can track and install dependencies with ease. Just run `bundle init` after you set up your project directory to create a `Gemfile`, where you list your gems.

Then we've got Ruby Version Manager (RVM) and Ruby Environment (rbenv), which help switch between Ruby versions per project. Pretty handy if you're juggling older code.

And what about frameworks? Ruby on Rails is the big name for web apps. But if you're going lightweight (like for services or APIs), check out Sinatra or Roda.

## See Also

- Ruby's official site for updates and documentation: [https://www.ruby-lang.org](https://www.ruby-lang.org)
- Bundler, to manage your Ruby gems: [https://bundler.io](https://bundler.io)
- RVM, a Ruby Version Manager: [https://rvm.io](https://rvm.io)
- rbenv, to pick a Ruby version for your project: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- Sinatra, a lightweight web framework: [http://sinatrarb.com](http://sinatrarb.com)
- For code sharing and collaboration, GitHub is your go-to: [https://github.com](https://github.com)
