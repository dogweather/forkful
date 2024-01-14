---
title:                "Fish Shell: 与yaml编程工作"
simple_title:         "与yaml编程工作"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

# 为什么会选择使用Fish Shell来编程

最近，有越来越多的人选择使用Fish Shell来编程，而其中一个原因就是它对YAML（YAML Ain't Markup Language）格式的支持。YAML是一种轻量级的标记语言，它可以用来存储和传输数据，而Fish Shell可以很容易地读取和处理这些数据。如果你经常使用YAML格式来存储配置文件或数据，在Fish Shell中编写程序将会是一个非常高效和方便的选择。

## 如何使用Fish Shell处理YAML格式的数据

首先，我们需要在Fish Shell中安装一个用于处理YAML的插件，比如[fy](https://github.com/fishpkg/fy)。安装完毕后，我们就可以在Fish Shell中直接使用`fy`命令来处理YAML格式的数据了。

假设我们有一个名为`sample.yml`的YAML文件，它的内容如下：

```yaml
person:
  name: "John"
  age: 27
  hobbies:
    - reading
    - hiking
    - coding
```

接下来，我们可以使用`fy get`命令来读取这个文件中的数据，比如我们想要获取`person`中的`name`：

```fish
fy get sample.yml person.name
```

运行结果会是`John`，非常简单快捷！除了`get`命令，`fy`还提供了其他一些命令来处理YAML数据，比如`set`可以用来设置数据，`search`可以用来搜索数据等等。通过阅读插件的[文档](https://github.com/fishpkg/fy#readme)，我们可以发现更多有用的命令和用法。

## 深入了解YAML格式和Fish Shell的结合

YAML格式的语法相对简单，它通过缩进来表示层级关系，而冒号来表示键和值的对应关系。在Fish Shell中，`fy`插件可以将这种语法转换成对应的数组和关联数组，方便我们进行数据处理。另外，Fish Shell还支持使用YAML作为配置文件，更加简洁美观，而且可以在Fish Shell中直接读取和使用这些配置数据，非常方便。

如果我们想要深入了解YAML格式和Fish Shell的结合，可以阅读[Fish Shell文档](https://fishshell.com/docs/current/)中有关YAML的部分，或者参考[fy](https://github.com/fishpkg/fy)插件的源码，来学习如何编写自定义的YAML处理函数。

## 参考链接

- [fy插件](https://github.com/fishpkg/fy)
- [Fish Shell文档](https://fishshell.com/docs/current/)
- [YAML规范](https://yaml.org/spec/1.2/spec.html)