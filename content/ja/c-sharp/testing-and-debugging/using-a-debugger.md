---
date: 2024-01-26 03:48:54.730715-07:00
description: "\u60F3\u50CF\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\u3001\u3042\
  \u306A\u305F\u306B\u306F\u6B63\u3057\u304F\u52D5\u4F5C\u3057\u3066\u3044\u306A\u3044\
  \u5C0F\u3055\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3042\u308A\u307E\u3059\uFF1A\
  \ ```C# static void Main() { int result = Sum(1, 2); Console.WriteLine(result);\
  \ } static int Sum(int a, int b) {\u2026"
lastmod: '2024-03-13T22:44:42.130036-06:00'
model: gpt-4-0125-preview
summary: "\u60F3\u50CF\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\u3001\u3042\
  \u306A\u305F\u306B\u306F\u6B63\u3057\u304F\u52D5\u4F5C\u3057\u3066\u3044\u306A\u3044\
  \u5C0F\u3055\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u3042\u308A\u307E\u3059\uFF1A\
  \ ```C# static void Main() { int result = Sum(1, 2); Console.WriteLine(result);\
  \ } static int Sum(int a, int b) {\u2026"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
想像してみてください、あなたには正しく動作していない小さなプログラムがあります：

```C#
static void Main()
{
    int result = Sum(1, 2);
    Console.WriteLine(result);
}

static int Sum(int a, int b)
{
    return a + a; // おっと、ここはa + bであるべきです
}
```

Visual Studioのデバッガーを使用して、`return a + a;`の隣にある左マージンをクリックしてブレークポイントを設定します。プログラムを実行するとき（F5キーで）、実行はそこで一時停止します。変数の上にマウスカーソルを置いて値を調べるか、または即時ウィンドウを使用して式を評価してみてください。`a` が1で、`b` が2であることがわかりますが、`a + a`は期待した合計ではありません。それを`a + b`に変えて、実行を続ける（F5キー）、そして、ほら、コンソールが3を出力します。

## ディープダイブ
デバッグの歴史は1940年代まで遡り、初期のコンピューターで本物のバグ（蛾）が発見されたときに始まります。今日のデバッガー、Visual Studioのものと同様、ブレークポイント、ステップバイステップ実行、ウォッチウィンドウなど、強力な機能のスイートを提供します。

Visual Studioのデバッガーに代わるオプションには、Cスタイルの言語用のGDBやPython用のpdbなどのオープンソースオプション、またC#および他の言語のデバッグツールを提供するクロスプラットフォームIDEであるJetBrains RiderやVS Codeが含まれます。

デバッガーの実装に深く飛び込むとき、あなたが見るのは、アプリケーションのプロセスにアタッチするプログラムです。それは機械語を解釈し、メモリ状態を管理し、実行フローを制御します。これは、効果的なデバッグのためには欠かせない重要なものであり、デバッグモードがリリースモードよりも遅く動作する理由でもあります。これらのフックが存在しないからです。

## 参照
- [Visual Studio デバッガー ドキュメント](https://docs.microsoft.com/en-us/visualstudio/debugger/)
- [デバッグ戦略](https://www.codeproject.com/Articles/79508/Effective-Exception-Handling-in-Visual-C)
