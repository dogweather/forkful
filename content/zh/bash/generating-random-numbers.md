---
title:    "Bash: 产生随机数"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## 为什么
Bash 编程是一种非常实用的技能，可以帮助您自动化任务并提高工作效率。生成随机数是一个有趣的挑战，它可以用于众多应用，例如游戏、密码生成和数据模拟。

## 如何做
如果您想学习如何使用 Bash 生成随机数，您可以按照以下步骤进行：

1. 打开终端并输入 `nano random_generator.sh` 命令来创建一个新的 Bash 脚本。
2. 在脚本中添加 `#!/bin/bash` 作为开头，这是告诉 shell 使用 Bash 解释器来运行脚本。
3. 添加 `echo $RANDOM` 行来生成随机数，并使用 `./random_generator.sh` 命令来运行脚本。
4. 每次运行脚本时，都会输出一个不同的随机数。您可以使用 `./random_generator.sh | head -5` 命令来输出前五个随机数。
5. 您也可以使用 `$RANDOM % n` 的语法来生成范围在 0 到 n 之间的随机数。例如，`echo $RANDOM % 100` 将会输出 0 到 99 之间的随机数。

```Bash
#!/bin/bash

echo $RANDOM
```

`18553`

```Bash
#!/bin/bash

echo $RANDOM % 100
```

`64`

## 深入了解
生成随机数并不是一件简单的事情，因为计算机是按照固定的规则运行的。所以，生成的随机数实际上是伪随机数，其结果是通过一个称为“随机数生成器”的算法来计算的。在 Bash 中，`$RANDOM` 变量实际上是一个伪随机数生成器，它会生成一个 0 到 32767 之间的数。

您也可以使用 `shuf` 命令来生成随机数，它使用的是比 Bash 更复杂的算法来计算随机数，并且可以定义范围和输出的数量。

## 另请参阅
- [Bash 编程入门指南 (英文)](https://www.gnu.org/software/bash/manual/bash.html#Generating-Numbers)
- [Bash 脚本教程 (英文)](https://ryanstutorials.net/bash-scripting-tutorial/bash-random.php)
- [shuf 命令文档 (英文)](https://www.man7.org/linux/man-pages/man1/shuf.1.html)