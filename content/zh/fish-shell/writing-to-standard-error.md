---
title:    "Fish Shell: 写入标准错误。"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 为什么

有时候当你运行脚本时，你可能会遇到一些错误。将这些错误信息打印到终端屏幕上不方便，因为它可能会干扰你的其他输出。将这些错误信息写入标准错误流（standard error）可以让你更方便地处理这些错误信息。 

## 使用方法

编程时，可以使用Fish Shell来将错误信息写入标准错误流。以下是一个简单的例子： 

```Fish Shell
# 创建一个文件夹
mkdir new_folder

# 如果文件夹已经存在，则将错误信息写入标准错误流
if test $status -ne 0
    echo "文件夹已经存在" >&2
end
```

上面的代码中，`mkdir`命令将文件夹创建的结果保存在变量`$status`中。如果变量的值不等于0，那么说明创建失败，随后的`if`语句会将错误信息写入标准错误流中。 

## 深入了解 

Fish Shell中有两个重要的输出流，分别是标准输出（standard output）和标准错误流。通过使用`&>`操作符，可以将标准输出和标准错误流合并为同一个流。这样做的好处是，不仅可以将错误信息打印到屏幕上，还可以将其保存在一个文件中方便以后查看。 

## 参考链接

- [Fish Shell文档](https://fishshell.com/docs/current/index.html)
- [了解标准输出和标准错误流](https://linux.cn/article-2800-1.html) 

## 参见

- [使用Fish Shell优化你的编程体验](https://linux.cn/article-11286-1.html)
- [学习Fish Shell的使用技巧](https://linux.cn/article-12446-1.html)