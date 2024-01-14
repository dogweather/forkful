---
title:    "PHP: 生成随机数"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数？

生成随机数在编程中是非常有用的，因为它们可以帮助我们在不同的场景下模拟真实的数据。例如，在游戏中生成随机地图或者在实验中使用随机数来测试假设。通过生成随机数，我们可以增加程序的灵活性和可复用性。

## 怎么做？

假设我们想要生成一个1到10之间的随机整数，那么可以使用 `rand()` 函数来完成。在下面的代码示例中，我们使用 `for` 循环来生成10个随机数，并将其打印出来。

```PHP
<?php
// 生成一个1到10之间的随机整数
for ($i = 1; $i <= 10; $i++) {
  $random_number = rand(1, 10);
  echo $random_number . "<br>";
}
?>
```

输出结果可能是：

```
6
2
9
3
8
4
7
1
5
10
```

## 深入了解

生成随机数并不是一件简单的事情，因为电脑是无法真正地生成完全随机的数字。它们只能使用算法来伪随机地生成数字。这也意味着相同的种子（seed）将会产生相同的随机数序列。因此，在某些情况下，我们可能需要使用不同的种子来确保产生不同的随机数序列。

另外，生成随机数也有一些比较常用的注意事项。例如，`rand()` 函数在某些情况下并不是完全均匀地随机生成数字，可以使用 `mt_rand()` 函数来解决这个问题。此外，如果需要更高质量的随机数，可以使用 `random_int()` 函数来生成加密级别的随机数。

## 参考资源

- [PHP官方文档：rand()](https://www.php.net/manual/zh/function.rand.php)
- [随机数生成详解](https://blog.csdn.net/aled512/article/details/11195761)
- [如何生成高质量的随机数](https://www.php.net/manual/zh/function.random-int.php)
- [如何控制生成随机数的质量](https://stackoverflow.com/questions/2240976/how-to-generate-high-quality-random-numbers-in-php)

## 参见

- [PHP 官方文档](https://www.php.net/manual/zh/)
- [PHP 中文网](https://www.php.cn/)