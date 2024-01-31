---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"

category:             "Ruby"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
写文本文件就是在电脑上创建和保存包含字符的文件。程序员需要这样做来存储配置、保存数据、或者记录应用程序日志。

## 如何操作:
```Ruby
# 在 Ruby 中创建并写入文件

# 打开文件并写入内容
File.open('example.txt', 'w') do |file|
  file.puts "Hello, Ruby!"
end

# 读取文件内容验证结果
puts File.read('example.txt')
```
输出:
```
Hello, Ruby!
```

## 深入理解:
写文本文件是编程的一个基本技能，存在已久，自从早期计算机时代就有了。交替的做法有数据库存储或者二进制文件，但文本文件简单、直观、易于调试。实现细节涉及文件系统调用和编码处理，例如 UTF-8。

## 参考:
- [Ruby 文件 I/O 官方文档](https://ruby-doc.org/core/File.html)
- [详细的 Ruby I/O 教程](https://www.tutorialspoint.com/ruby/ruby_input_output.htm)
- [关于编码的更多信息](https://ruby-doc.org/core-2.7.1/Encoding.html)
