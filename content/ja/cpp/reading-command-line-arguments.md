---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
html_title:           "C++: コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み取ることの重要性について最大2文で説明します。

## 方法
コマンドライン引数を読み取るには、`argc`と`argv`という2つのパラメータを使用します。`argc`はコマンドラインに入力された引数の数を保持し、`argv`は引数が格納された配列です。以下の例をご覧ください。

```C++
#include <iostream>

int main(int argc, char *argv[]) {
  // プログラム名を出力
  std::cout << "プログラム名: " << argv[0] << std::endl;

  // 引数を出力
  for (int i = 1; i < argc; i++) {
    std::cout << "引数" << i << ": " << argv[i] << std::endl;
  }
  
  return 0;
}
```

### 例1:
コマンドラインに入力された引数がない場合、出力は以下のようになります。

```
プログラム名: テストプログラム
```

### 例2:
コマンドラインに`hello world`という引数が入力された場合、出力は以下のようになります。

```
プログラム名: テストプログラム
引数1: hello
引数2: world
```

## 詳細
コマンドライン引数を読み取ることで、プログラムを柔軟に設計することができます。例えば、ユーザーにプログラムの動作を制御する引数を与えることができます。また、コマンドライン引数は複数のプログラムを実行する際にも便利です。

## その他
* [コマンドライン引数 - C言語リファレンス](https://www.javadrive.jp/cstart/argv/index1.html)
* [コマンドライン引数を使ってみる - C++入門](http://cpp-lang.sevendays-study.com/language/51.html)

---

## 参考
* [Markdown記法 - Qiita Help](https://help.qiita.com/ja/articles/markdown-guide)
* [Markdown記法 サンプル集 - Qiita](http://qiita.com/Qiita/items/c686397e4a0f4f11683d)