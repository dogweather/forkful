---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ?

プロジェクトの新設は、特定の目標を達成するために始めるプログラムのセットのことを指します。プログラマーは新しいアイデアを探索し、必要な機能を追加、改善するために、新しいプロジェクトを始めます。

## どうやって:

Clojureで新しいプロジェクトを始めるには、Leiningenという素晴らしいツールを利用します。以下にその方法を示します。

```Clojure
;; Leiningenのインストール
$ brew install leiningen

;; 新しいプロジェクトの作成
$ lein new my-first-project
```

これで、'my-first-project'という新しいClojureプロジェクトが作成されます。

## ディープダイブ

Clojureのプロジェクトを新規作成することについて、その歴史的背景、代替手段、具体的な実装について深掘りします。

1. **歴史的背景**: Clojureは2007年にRich Hickeyによって作られ、関数型プログラミング言語として一世を風靡しました。新プロジェクトの作成と管理のためにLeiningenが開発されました。

2. **代替手段**: Leiningen以外にも、BootやClojure CLIなど、他のプロジェクト管理ツールを利用することも可能です。

3. **実装詳細**: 新しいプロジェクトを作る際、Clojureはプロジェクトのテンプレートを作成し、それに自動的にプロジェクトの基本的な構造を適用します。

## 参考資料

1. [Official Clojure Documentation](https://clojure.org/)
2. [Leiningen Documentation](https://leiningen.org/)
3. [Boot Documentation](https://boot-clj.com/)
4. [Clojure CLI User Guide](https://clojure.org/guides/deps_and_cli)