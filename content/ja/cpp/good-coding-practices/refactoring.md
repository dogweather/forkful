---
title:                "リファクタリング"
aliases: - /ja/cpp/refactoring.md
date:                  2024-01-26T01:17:33.773361-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/refactoring.md"
---

{{< edit_this_page >}}

## 何となくその理由？

リファクタリングは、コンピュータプログラムの内部構造を変更しながらも、その外部の動作を変えないプロセスです。プログラマーは、コードを綺麗にし、理解しやすく、維持しやすく、拡張しやすくするためにこれを行います。

## 方法：

あまりにも多くのことを行っている関数があると想像してみてください。例えば、オブジェクトを初期化し、ログ記録も行うこの不格好なメソッドのように：

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // 初期化ロジック
        // ...

        // 詳細ログ
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// 使用方法：
Widget w;
w.init(true);
```

出力：
```
Widget initialized!
```

これをよりクリーンで、焦点を絞ったメソッドにリファクタリングすると、次のようになるかもしれません：

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // 初期化ロジックのみ
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// 使用方法：
Widget w;
w.init();
w.logInitialization();
```

この変更はプログラムが何をするかを変えてはいませんが、`Widget` クラスをよりモジュラーにし、その使用法をより明確にします。

## 深掘り

今日私たちが知っているリファクタリングの概念は、1980年代のSmalltalkプログラミングコミュニティでのルーツを持ち、1999年に発表されたマーティン・ファウラーの著書「Refactoring: Improving the Design of Existing Code」によって大きく普及しました。今日では、リファクタリングは現代のソフトウェア開発の中核部分であり、アジャイルやTDD（テスト駆動開発）など様々な開発方法論に組み込まれています。

リファクタリングの代替と言えば、リライティングやリデザインへと話が向かいます。リファクタリングは戦略的で段階的ですが、リライトは既存のコードを捨てて新しいソリューションに移行する場合があります。一方で、リデザインは機能を変更することを含むより大幅な変更を伴うことがあり、純粋なリファクタリングの非目標です。

リファクタリングの実装についてはかなり細かくなることがあります。長いメソッド、大きなクラス、または重複したコードなど、リファクタリングを促す「コードのにおい」が多数存在します。C++用の「Clang-Tidy」など、問題を特定し、いくつかの修正を適用できる自動化ツールが存在します。

その上、リファクタリングには機能が変わらないことを確認するために確かなテストスイートが必要です。テストがなければ、基本的に目隠しで飛んでいるようなもので、リグレッションをリスクにさらしています。

## 参照

リファクタリングについてのさらなる理解と、より多くの例を見るために、次の点をチェックしてみてください：

- 基本的なアイデアと戦略のためのマーチン・ファウラーの古典的なテキスト「Refactoring: Improving the Design of Existing Code」。
- C++での自動リファクタリングサポートについては、https://clang.llvm.org/extra/clang-tidy/ での `Clang-Tidy` のドキュメント。
- 完璧ではない既存のコードベースの文脈で安全にリファクタリングを行う技術を提供するマイケル・フェザーズの「Working Effectively with Legacy Code」。
