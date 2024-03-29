---
date: 2024-01-27 20:34:55.362599-07:00
description: "\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\u521B\u5EFA\u4E0D\u53EF\u4EE5\
  \u6BD4\u968F\u673A\u66F4\u597D\u5730\u88AB\u9884\u6D4B\u7684\u6570\u5B57\uFF0C\u8FD9\
  \u5BF9\u4E8E\u5F00\u53D1\u6A21\u62DF\u3001\u6E38\u620F\u548C\u5B89\u5168\u7B97\u6CD5\
  \u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u5728\u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5F15\u5165\u4E0D\u53EF\u9884\
  \u6D4B\u6027\u6216\u6A21\u62DF\u73B0\u5B9E\u4E16\u754C\u73B0\u8C61\u3002"
lastmod: '2024-03-13T22:44:47.250464-06:00'
model: gpt-4-0125-preview
summary: "\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\u521B\u5EFA\u4E0D\u53EF\u4EE5\
  \u6BD4\u968F\u673A\u66F4\u597D\u5730\u88AB\u9884\u6D4B\u7684\u6570\u5B57\uFF0C\u8FD9\
  \u5BF9\u4E8E\u5F00\u53D1\u6A21\u62DF\u3001\u6E38\u620F\u548C\u5B89\u5168\u7B97\u6CD5\
  \u81F3\u5173\u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\
  \u5728\u4ED6\u4EEC\u7684\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5F15\u5165\u4E0D\u53EF\u9884\
  \u6D4B\u6027\u6216\u6A21\u62DF\u73B0\u5B9E\u4E16\u754C\u73B0\u8C61\u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么?

生成随机数涉及创建不可以比随机更好地被预测的数字，这对于开发模拟、游戏和安全算法至关重要。程序员这样做是为了在他们的应用程序中引入不可预测性或模拟现实世界现象。

## 如何操作：

Python提供了`random`模块，有助于生成多种用途的随机数。以下是入门方式：

1. **导入模块**
    ```Python
    import random
    ```

2. **生成随机整数**
    在任意两个数字之间。
    ```Python
    random_integer = random.randint(1, 10)
    print(random_integer)
    ```
    示例输出：`7`

3. **生成浮点数**
    在0和1之间。
    ```Python
    random_float = random.random()
    print(random_float)
    ```
    示例输出：`0.436432634653`

    如果你需要在不同范围内的浮点数，可以乘以：
    ```Python
    random_float_range = random.random() * 5  # 0到5
    print(random_float_range)
    ```
    示例输出：`3.182093745`

4. **从列表中挑选随机元素**
    ```Python
    greetings = ['Hello', 'Hi', 'Hey', 'Hola', 'Bonjour']
    print(random.choice(greetings))
    ```
    示例输出：`Hola`

5. **洗牌列表**
    对于需要随机化顺序的纸牌游戏或任何应用程序来说都很完美。
    ```Python
    numbers = list(range(10))
    random.shuffle(numbers)
    print(numbers)
    ```
    示例输出：`[2, 5, 0, 4, 9, 8, 1, 7, 6, 3]`

## 深入了解

Python的`random`模块使用伪随机数生成器（PRNG），特别是Mersenne Twister算法，这适用于一般用途应用，但由于如果观察到足够多的输出就可以预测，所以不适合用于加密目的。Python 3.6引入的`secrets`模块，为生成密码学上强随机数提供了更好的选择，特别是在安全敏感的应用程序中非常有用。例如，生成安全的、随机的密码重置链接令牌：

```Python
import secrets
token = secrets.token_hex(16)
print(token)
```

历史上，生成真正随机的随机数在计算中一直是一个挑战，早期方法依赖于物理现象或手动输入的种子。像Mersenne Twister这样的算法的开发和采用（至少在2023年我最后知识更新时，Python的`random`模块默认使用）标志着显著进步。然而，为了找寻更安全和高效的算法的持续搜索，导致了包括`secrets`模块以处理与密码学相关的任务。这种进化反映了软件开发中安全的日益重要性，以及从加密到安全令牌生成等应用中对更强大的随机性的需求。
