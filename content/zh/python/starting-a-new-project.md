---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Python"
category:             "Python"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？

新项目的开始就是创建一个新的编程任务，程序员通过它可以解决特定的问题或达到一个特定的目标。程序员之所以这么做，一方面是因为编程可以使物理世界的事物数字化，另一方面也能帮助改进和优化系统。
                                                                                                               
## 如何?

```python
# 首先，创建一个新的Python文件
# 在Python中，我们使用.py作为文件的扩展名

# 该文件中的内容可以包括变量、函数、类等
variable = "Hello, World!"

def function():
    print(variable)
    
class MyClass:
    def method(self):
        print(variable)
```
此代码创建了一个新的Python文件，其中包含一个变量、一个函数和一个类。运行这段代码会得到以下结果：

```python
>>> function()
Hello, World!

>>> obj = MyClass()
>>> obj.method()
Hello, World!
```
## 深入

#### 历史背景
创建新项目在程序设计的早期阶段就已经存在。早期，程序员必须从零开始编程，现在的编程语言和工具能够更简洁有效地开启新的项目。

#### 可选方法
除了以上所述，存在许多其他创建新项目的方法。例如，很多现代的集成开发环境（IDE）包含用于创建新项目的模板和向导。

#### 关于实施
Python是一种理想的语言来开始新的项目，因为它既适合初学者，又能满足复杂项目的需求。你可以在任何支持Python的文本编辑器中开启新项目。

## 参见：

- [Python官方文档](https://docs.python.org/3/tutorial/index.html)
- [创建Python项目的步骤步教程](https://realpython.com/tutorials/projects/)
- [如何使用PyCharm开启新项目](https://www.jetbrains.com/help/pycharm/creating-and-running-your-first-python-project.html)