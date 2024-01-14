---
title:                "C++: 生成随机数"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：生成随机数有什么用处？
生成随机数在编程中的作用十分重要。它可以用于测试程序的稳定性和边缘情况，也可以用于创建随机模拟实验。此外，随机数也被用于创建唯一的ID或者密码，并且在游戏开发中也十分常见。

## 如何生成随机数

```C++
#include <iostream>
#include <cstdlib> //包含了rand()和srand()函数
#include <ctime>   //包含了time()函数，用于设置seed值

using namespace std;

int main(){
    
    //设置seed值，可以保证每次运行程序时生成的随机数是不同的
    srand(time(0));
    
    //生成一个介于0到99之间的随机数
    int num = rand() % 100;
    
    //打印随机数
    cout << "随机数为：" << num << endl;
    
    return 0;
}
```

输出结果：

```
随机数为：42
```

更复杂的情况下，我们也可以生成一个指定范围内的随机数，比如生成一个1到10之间的随机数。

```C++
//生成一个介于1到10之间的随机数
int num = 1 + rand() % 10;
```

我们也可以生成一个浮点数，比如生成一个0到1之间的随机小数。

```C++
//生成一个0到1之间的随机小数
float num = rand() / (float)RAND_MAX;
```

## 深入了解

在生成随机数的过程中，还有一些概念需要我们深入了解。首先是seed值，它决定了随机数的起点，也就是说如果seed值相同，那么生成的随机数也将是相同的。通常情况下，我们可以使用当前的时间作为seed值，这样每次重新运行程序时都会有不同的seed值。

其次是伪随机数。由于计算机的算法实现的原因，所谓的随机数并不是真正意义上的随机，而是伪随机。这意味着生成的随机数有一定的规律可循，但对于一般的应用来说已经足够。

最后是随机数的分布。比如使用rand()函数所生成的随机数，其分布并非是均匀的，也就是说不同的数出现的概率并不相同。如果需要均匀分布的随机数，可以使用一些其他的算法实现。

## 参考资料

- [C++ 随机数生成](https://zh.cppreference.com/w/cpp/numeric/random)
- [C 深入理解伪随机数生成算法](https://baike.baidu.com/item/%E4%BC%AA%E9%9A%8F%E6%9C%BA%E6%95%B0%E6%8D%AE%E3%80%81%E6%9C%BA%E7%A8%8B%E7%94%9F%E6%88%90%E7%AE%97%E6%B3%95)
- [轻松学习 C++ 中的随机数生成机制](https://www.jianshu.com/p/bc64542c885b)

## 参见

- [C++ for Beginners](https://www.programiz.com/cpp-programming)
- [如何使用随机数进行游戏开发](https://www.gamedev.net/tutorials/programming/general-and-gameplay-programming/generating-random-numbers-r1174/)
- [随机数的应用场景](http://www.ccrint.com/algorithm/rand01/)