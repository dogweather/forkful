---
title:    "Ruby: 生成随机数"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## 为什么
随机数生成是编程中经常用到的工具，它可以帮助我们创建虚拟的数据，用于测试和模拟。此外，它也被用于游戏开发、加密和随机化算法。

## 如何操作
有几种方法可以在Ruby中生成随机数，我们将通过下面的代码块来介绍它们：

```Ruby
# 生成范围内的随机整数
puts rand(1..10)

# 生成0-1之间的随机小数
puts rand()

# 给定种子生成随机数
srand(1234)
puts rand(1..10)
```

以下是输出结果的示例：

```
5
0.7688192234080053
9
```

我们可以使用rand方法来生成范围内的随机整数，可以使用rand方法不指定范围来生成0-1之间的随机小数，也可以使用srand方法给定一个种子来生成可预测的随机数。

## 深入探索
随机数生成是一个很有意思的话题，它涉及到伪随机数算法、种子以及如何创建真正的随机数。Ruby中的rand方法实际上是一个伪随机数算法，它根据可以预测的种子来生成一系列的数字。因此，在某些情况下，为了避免数据重复，我们可以使用srand方法来指定一个不同的种子。而要创建真正的随机数，则需要依靠硬件设备，比如随机数发生器。

## 参考资料
- http://ruby-doc.org/core-2.7.1/Random.html
- https://www.rubyguides.com/2019/10/ruby-random/ 
- https://ruby-doc.org/stdlib-2.6.3/libdoc/securerandom/rdoc/SecureRandom.html

## 参见
- [Ruby文档](https://ruby-china.org/wiki/ruby-manual)
- [Ruby社区](https://ruby-china.org/)
- [Ruby on Rails](https://rubyonrails.org/)