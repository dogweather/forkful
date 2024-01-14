---
title:    "PHP: 读取命令行参数"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# 为什么要阅读命令行参数

阅读命令行参数是编写高效可靠的程序的必要技能。通过理解如何读取和使用命令行参数，您可以轻松地修改程序的行为从而使其更加灵活和可扩展。

## 如何读取命令行参数

命令行参数是指在程序运行时通过命令行传递给程序的额外信息。在PHP中，您可以通过使用`$argv`和`$argc`变量来读取命令行参数。`$argv`是一个包含所有命令行参数的数组，索引从1开始计数。而`$argc`是一个整数变量，表示命令行参数的个数。

```PHP
// 假设该程序名为example.php
php example.php arg1 arg2 arg3

// example.php的代码如下：
<?php
// 读取命令行参数，并存储到数组中
$params = $argv;
// 获取命令行参数的个数
$count = $argc;
// 输出参数的值和个数
echo "以下是您传递给程序的参数：";
for ($i = 1; $i < $count; $i++) {
    echo $params[$i];
}
echo "共有 {$count} 个参数";
```
输出结果如下：
```
以下是您传递给程序的参数：arg1arg2arg3共有 4 个参数
```

## 深入理解命令行参数

除了使用`$argv`和`$argc`变量，您还可以使用PHP内置函数`getopt()`来读取命令行参数。`getopt()`函数允许您在参数前添加单破折线`-`和双破折线`--`来标识参数的长短形式，同时还可以指定参数是否需要接收值。这样可以使得命令行参数更加易读和易用。

```PHP
// 假设该程序名为example.php
php example.php -f file.txt -h --help

// example.php的代码如下：
<?php
// 通过getopt()函数来读取命令行参数
$options = getopt("f:h", ["help"]);
// 判断是否存在文件参数
if (isset($options['f'])) {
    $filename = $options['f'];
    echo "您指定的文件名是{$filename}";
}
// 判断是否存在帮助参数
if (isset($options['h']) || isset($options['help'])) {
    echo "这是一个帮助信息";
}
```
输出结果如下：
```
您指定的文件名是file.txt
这是一个帮助信息
```

# 参考资料

- [PHP官方文档 - 命令行参数](https://www.php.net/manual/zh/reserved.variables.argv.php)
- [PHP官方文档 - getopt()函数](https://www.php.net/manual/zh/function.getopt.php)
- [阮一峰的网络日志 - getopt函数（命令行参数处理）](https://www.ruanyifeng.com/blog/2015/05/getopt.html)

# 参阅

- [廖雪峰的官方网站 - 命令行程序入门](https://www.liaoxuefeng.com/wiki/896043488029600)
- [PHP中文网 - 如何通过命令行运行PHP程序](https://www.php.cn/php-weizijiaocheng-403354.html)