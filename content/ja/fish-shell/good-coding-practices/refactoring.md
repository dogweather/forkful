---
date: 2024-01-26 01:18:29.294706-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u5916\u90E8\
  \u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\u30B3\
  \u30FC\u30C9\u3092\u518D\u69CB\u9020\u5316\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u975E\u6A5F\u80FD\u5C5E\u6027\u306E\u6539\
  \u5584\u3092\u76EE\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\
  \u3055\u305B\u3001\u5C06\u6765\u7684\u306B\u30B9\u30B1\u30FC\u30EB\u30A2\u30C3\u30D7\
  \u307E\u305F\u306F\u4FEE\u6B63\u304C\u5BB9\u6613\u306B\u306A\u308B\u3088\u3046\u306B\
  \u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.749040-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306F\u3001\u5916\u90E8\
  \u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\u30B3\
  \u30FC\u30C9\u3092\u518D\u69CB\u9020\u5316\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u975E\u6A5F\u80FD\u5C5E\u6027\u306E\u6539\
  \u5584\u3092\u76EE\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u8907\u96D1\u3055\u3092\u6E1B\u3089\u3057\u3001\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\
  \u3055\u305B\u3001\u5C06\u6765\u7684\u306B\u30B9\u30B1\u30FC\u30EB\u30A2\u30C3\u30D7\
  \u307E\u305F\u306F\u4FEE\u6B63\u304C\u5BB9\u6613\u306B\u306A\u308B\u3088\u3046\u306B\
  \u3001\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 何を、なぜ？
リファクタリングは、外部の振る舞いを変えずに既存のコードを再構造化するプロセスです。これにより非機能属性の改善を目指します。プログラマーは、コードをより読みやすく、複雑さを減らし、保守性を向上させ、将来的にスケールアップまたは修正が容易になるように、リファクタリングを行います。

## 方法：
時間が経つにつれてかなり成長してしまったスクリプトがあると想像してください。始まりはシンプルでしたが、現在は論理の触手で広がる獣になっています。以下は、関数をより読みやすく効率的にするためのリファクタリングの一例です：

リファクタリング前：
```fish
function old_and_clunky
    set color (cat ~/.config/fish/color_theme)
    if test "$color" = 'blue'
        echo 'Blue theme set!'
    else if test "$color" = 'red'
        echo 'Red theme set!'
    else
        echo 'Default theme set!'
    end
end
```

リファクタリング後：
```fish
function set_theme_color
    set theme_color (cat ~/.config/fish/color_theme)
    switch $theme_color
        case blue
            echo 'Blue theme set!'
        case red
            echo 'Red theme set!'
        default
            echo 'Default theme set!'
    end
end
```
リファクタリングにより、関数の名前がその目的をよりよく示すように改善され、if-else連鎖がよりクリーンな`switch`ステートメントに置き換えられました。

サンプル出力：
```
Blue theme set!
```

## 深掘り
リファクタリングは最初、マーチン・ファウラーの画期的な書籍「Refactoring: Improving the Design of Existing Code」で詳細に説明されました。この書籍は、新しい機能を書くことなくコードを改善する構造化されたアプローチを示しました。それ以来、多くのリファクタリング技術が導入され、この概念は現代のソフトウェア開発の基本的な部分となっています。

Fish Shell環境におけるリファクタリングは、専用の構文とコマンドラインの性質のため、他のプログラミングの文脈とは若干異なるかもしれません。Fishにおけるスクリプトのリファクタリングの代替方法には、別のシェル言語への移植や、より高度なスクリプト管理のための外部ツールの使用が含まれることがあります。しかし、ネイティブのFish構文を保持することは、シェルの機能とのより良い統合を意味し、全体としてより洗練された体験を意味します。

Fish Shellでのリファクタリングでは、他の言語の広範囲にわたるクラスやモジュールとは対照的に、主に関数とコマンドを扱います。この細かさはリファクタリングのタスクをより直接的で即座なプロセスにすることができますが、それはまた、明確で簡潔で、保守可能なコードの重要性を強調します。

## 参照
- マーチン・ファウラーのリファクタリングウェブサイト：[https://refactoring.com/](https://refactoring.com/)
- 公式Fish Shellドキュメント：[https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
