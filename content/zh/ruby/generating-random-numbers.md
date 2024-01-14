---
title:                "Ruby: 产生随机数"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：为什么要使用Ruby生成随机数？生成随机数是一种常用的技术，它可以用来模拟真实世界中的随机事件，例如抽奖、游戏、安全验证等。

如何操作：编码示例和代码块"```Ruby ...```"内的输出示例。

```
# 生成一个随机整数
rand(1..10)
# 输出可能的结果：1, 2, 3, 4, 5, 6, 7, 8, 9, 10

# 生成一个随机小数
rand(0.0..1.0)
# 输出可能的结果：0.5145940986159021, 0.23797436272529372, 0.8050768642132238, 0.6949758396823401, 等等

# 从数组中随机选择一个元素
["苹果", "香蕉", "橘子", "葡萄"].sample
# 输出可能的结果：苹果、香蕉、橘子、葡萄中的其中一个
```

深入了解：生成随机数的过程实际上并不是完全随机的。它是通过一个数学算法来计算的，这个算法使用一个种子数作为输入，然后根据这个种子数来计算出随机数。因此，如果使用相同的种子数，就会得到同样的随机数序列，这也是为什么我们在生成随机数时会给一个种子数的原因。另外，使用伪随机数生成器可以提高计算性能，因为它们不需要消耗额外的资源来生成随机数。

此外，Ruby还提供了一些其他有用的方法来生成随机数，例如SecureRandom模块，它可以生成更具安全性的随机数，适用于安全验证等场景。

### 参考链接

- [Ruby文档 - 随机数生成器](https://ruby-doc.org/core-2.7.2/Random.html)
- [Ruby文档 - SecureRandom模块](https://ruby-doc.org/stdlib-2.7.2/libdoc/securerandom/rdoc/SecureRandom.html)
- [Tutorialspoint - 随机数生成器](https://www.tutorialspoint.com/ruby/ruby_random_numbers.htm)
- [Thoughtbot - 用Ruby实现随机行走](https://thoughtbot.com/blog/random-walks-with-ruby)