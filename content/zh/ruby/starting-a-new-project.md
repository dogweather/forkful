---
title:                "开始一个新项目"
html_title:           "Ruby: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么？为什么？
开展一个新项目是指开始一个新的编码或软件开发项目。程序员会这样做是因为他们要创建新的应用程序，解决特定的问题，或者改进现有的项目。

## 如何：
```Ruby
#创建一个新的项目
rails new my_project

#打印项目名称
puts "My project is called my_project"

#创建一个新的类
class NewProject
  attr_accessor :name
  
  #定义初始化方法
  def initialize(name)
    @name = name
  end
  
  #打印项目名称方法
  def print_name
    puts "#{@name} is my new project"
  end
end

#实例化新的项目
new_project = NewProject.new("My Project")

#调用打印项目名称方法
new_project.print_name
```

输出结果：
```
My project is called my_project
My project is my new project
```

## 深入了解：
开展新项目并不是什么新鲜事。自从计算机发明以来，程序员们一直在不断地创建新的软件和应用程序。除了Ruby，还有其他编程语言也可以用来开展新的项目，比如Python，Java等。实际上，因为Ruby是一门应用程序开发的高级语言，它被广泛地用于开发新项目。

## 参考资料：
- Ruby on Rails官方网站：https://rubyonrails.org/
- Ruby编程语言官方网站：https://www.ruby-lang.org/zh_cn/
- 使用Ruby开发新项目的步骤：https://www.tutorialspoint.com/ruby/ruby\_quick\_guide.htm