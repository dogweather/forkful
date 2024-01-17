---
title:                "使用yaml 进行编程"
html_title:           "Go: 使用yaml 进行编程"
simple_title:         "使用yaml 进行编程"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么是 YAML? 
YAML是一种轻量级的可读性高的数据交互格式，经常被程序员使用来存储和传输数据。它很容易阅读和书写，因此成为了很多项目的首选格式。有些项目甚至使用YAML本身作为配置文件，而不是使用其他格式。

## 为什么会有YAML？ 
程序员使用YAML的主要原因是它的可读性和易用性。与其他格式相比，YAML文件更加简洁、整洁，易于解析和编辑。此外，它还具有可扩展性，可以表示更复杂的数据结构。

## 如何使用？ 
```Go 
import "gopkg.in/yaml.v2" 

type User struct { 
	Name string `yaml:"name"` 
	Age int `yaml:"age"` 
	Email string `yaml:"email"` 
} 

func main() { 
	user := User{ 
		Name: "John", 
		Age: 27, 
		Email: "john@email.com", 
	} 
	yamlData, _ := yaml.Marshal(user) 
	fmt.Println(string(yamlData)) 
} 
``` 
输出： 
``` 
name: John 
age: 27 
email: john@email.com 
``` 

## 深入了解 
YAML最初在2001年由Clark Evans和Ingy döt Net提出，旨在解决XML和负责完全的数据格式之间的折中。它已经成为主流的数据格式之一，被许多语言和框架支持。如果不想使用YAML，JSON也是一个不错的选择。在使用YAML时，需要注意的是其缩进规则及其区别对待字符串和其他类型的数据的方式。

## 相关资料 
- [YAML官方网站](https://yaml.org/) 
- [Go语言中使用YAML的文档](https://godoc.org/gopkg.in/yaml.v2)