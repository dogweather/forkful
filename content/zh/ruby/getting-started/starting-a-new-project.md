---
title:                "开始一个新项目"
date:                  2024-01-20T18:04:38.496583-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
新建项目是创建全新的代码库，从零开始。程序员这样做是为了实现新的想法，解决特定问题或响应需求。

## 如何去做：
```Ruby
# 初始化一个新的Ruby项目
# 创建项目目录
Dir.mkdir("my_new_project")

# 进入项目目录
Dir.chdir("my_new_project") do
  # 初始化Gemfile
  File.write("Gemfile", "source 'https://rubygems.org'\n")

  # 安装Bundler
  system("gem install bundler")

  # 使用Bundler初始化项目
  system("bundle init")
  
  # 创建主程序文件
  File.write("main.rb", "#!/usr/bin/env ruby\n\nputs 'Hello, new project!'")
end

# 输出示例
# 运行主程序文件
puts `ruby my_new_project/main.rb`
```
Output:
```
Hello, new project!
```

## 深入探究：
新建项目在Ruby的历史中一直很简单。与其它语言相比，Ruby强调了“乐趣编程”（"the joy of programming"），所以工具和流程都尽可能的直接简便。在过去，你可能使用像`rails new`这样的命令开始一个Ruby on Rails项目，或者使用Sinatra框架，直接写一个文件启动。如今，我们有Bundler和Gemfile，它们帮助我们管理依赖。当涉及到部署或发布项目，RubyGems和Bundler也提供了相应的支持。

Ruby项目中，传统上我们会创建一个README文件，介绍项目用途和使用方式，可能还包括LICENSE文件说明版权和使用条款。虽然现在有许多工具和云服务为Ruby项目提供支持，但Ruby社区仍鼓励简洁、清晰和直观的代码和工程实践。

## 另见：
- [Ruby新手指南](https://www.ruby-lang.org/zh_cn/documentation/quickstart/)
- [Bundler官网](https://bundler.io/)
- [RubyGems官网](https://rubygems.org/)
- [Rails指南：开始](https://guides.rubyonrails.org/getting_started.html)
- [Sinatra入门](http://sinatrarb.com/intro.html)
