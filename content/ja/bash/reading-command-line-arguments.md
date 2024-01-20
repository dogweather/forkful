---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、ユーザーがプログラムに情報を提供する方法です。これにより、プログラムは動的に動作し、多くの状況で使用することができます。

## どのように:

以下にコマンドライン引数の読み取りの基本的な例を示します。

```Bash
#!/bin/bash
echo "スクリプト名: $0"
echo "最初の引数: $1"
echo "二つ目の引数: $2"
```

引数を次のように提供し、スクリプトを実行します:

```Bash
./myscript.sh arg1 arg2
```

出力は次のようになります:

```Bash
スクリプト名: ./myscript.sh
最初の引数: arg1
二つ目の引数: arg2
```

すべての引数を読み取るには、"$@"を使用します:

```Bash
#!/bin/bash
for arg in "$@"; do
    echo "引数: $arg"
done
```

## 深掘り

コマンドライン引数の概念は、Unixの初期から存在していました。これはプログラムが汎用性を持つことを可能にし、パイプラインで他のプログラムと組み合わせることができます。

他の方法としては、環境変数の使用がありますが、これは限定的なケースで使用され、一般的にはコマンドライン引数の方が優先されます。

## 参考文献

- Advanced Bash-Scripting Guide: [http://tldp.org/LDP/abs/html/](http://tldp.org/LDP/abs/html/)
- Bash Manual: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)