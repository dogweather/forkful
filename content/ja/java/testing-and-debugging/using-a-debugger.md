---
date: 2024-01-26 03:50:15.106700-07:00
description: "\u4F8B\u3048\u3070\u3001\u4F55\u304B\u304A\u304B\u3057\u3044Java\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u304C\u3042\u308A\u3001\u305D\u306E\u7406\u7531\u304C\u308F\
  \u304B\u3089\u306A\u3044\u3068\u3057\u307E\u3059\u3002Eclipse\u306E\u3088\u3046\u306A\
  \u4EBA\u6C17\u306E\u3042\u308BJava\u958B\u767A\u7528IDE\u3092\u4F7F\u3063\u3066\u30C7\
  \u30D0\u30C3\u30AC\u30FC\u3092\u8D77\u52D5\u3059\u308B\u65B9\u6CD5\u306F\u4EE5\u4E0B\
  \u306E\u901A\u308A\u3067\u3059:\u2026"
lastmod: '2024-03-13T22:44:41.953967-06:00'
model: gpt-4-0125-preview
summary: "\u4F8B\u3048\u3070\u3001\u4F55\u304B\u304A\u304B\u3057\u3044Java\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u304C\u3042\u308A\u3001\u305D\u306E\u7406\u7531\u304C\u308F\u304B\
  \u3089\u306A\u3044\u3068\u3057\u307E\u3059\u3002Eclipse\u306E\u3088\u3046\u306A\u4EBA\
  \u6C17\u306E\u3042\u308BJava\u958B\u767A\u7528IDE\u3092\u4F7F\u3063\u3066\u30C7\u30D0\
  \u30C3\u30AC\u30FC\u3092\u8D77\u52D5\u3059\u308B\u65B9\u6CD5\u306F\u4EE5\u4E0B\u306E\
  \u901A\u308A\u3067\u3059:\n\n\u307E\u305A\u3001\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\
  \u30F3\u30C8\u3092\u8A2D\u5B9A\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\
  \u3057\u3066\u304F\u3060\u3055\u3044\u3002\u6B21\u306B\u3001\u30D5\u30A1\u30A4\u30EB\
  \u3092\u53F3\u30AF\u30EA\u30C3\u30AF\u3057\u3001\u300C\u30C7\u30D0\u30C3\u30B0\u3068\
  \u3057\u3066\u5B9F\u884C\u300D\u3092\u9078\u629E\u3057\u3001\u305D\u3057\u3066\u300C\
  Java\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u300D\u3092\u30AF\u30EA\u30C3\
  \u30AF\u3057\u307E\u3059\u3002\n\n```Java\npublic class DebugExample {\n    public\
  \ static void main(String[] args) {\n        int a = 5;\n        int b = 0;\n  \
  \      // \u3053\u3053\u306B\u30D6\u30EC\u30FC\u30AF\u30DD\u30A4\u30F3\u30C8\u3092\
  \u8A2D\u5B9A\u3057\u307E\u3059\n        int result = divide(a, b);\n        System."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
例えば、何かおかしいJavaプログラムがあり、その理由がわからないとします。Eclipseのような人気のあるJava開発用IDEを使ってデバッガーを起動する方法は以下の通りです:

まず、ブレークポイントを設定していることを確認してください。次に、ファイルを右クリックし、「デバッグとして実行」を選択し、そして「Javaアプリケーション」をクリックします。

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // ここにブレークポイントを設定します
        int result = divide(a, b);
        System.out.println("結果は: " + result);
    }

    private static int divide(int numerator, int denominator) {
        // 別のブレークポイントを設定するのに良い場所
        return numerator / denominator;
    }
}
```

これを行うと、プログラムはブレークポイントで一時停止し、変数を検査したり、コードを一行ずつ進めたり、プログラムの動作を観察したりできます。

デバッガーコンソールでのサンプル出力:
```
ブレークポイントがヒットしました: int result = divide(a, b);
```

## 深掘り
デバッグの概念はプログラミングの初期から存在しています。伝説によれば、「バグ」という言葉は、分野の先駆者であるグレース・ホッパーがコンピューター内に実際にいたモスのような虫を見つけたことに由来すると言われています。今日に至るまで、IntelliJ IDEA, Eclipse, NetBeansといった強力なデバッガーを搭載した洗練されたIDEがあります。

IDEデバッガーの代替手段には、ログ記録、print文（貧乏人のデバッガー）、アサーション、Java Development Kit (JDK)の一部であるjdb（Java Debugger）のようなスタンドアローンのデバッグツールが含まれます。

デバッガーは、プログラマーが実行を一時停止（ブレークポイント）、コードを逐次実行、変数の値を検査、その値をその場で変更、さらにはコードブロックをブロックごとに実行することを可能にすることで機能します。デバッガーの使用は、問題を引き起こしている正確なコード行を見つけることが干し針を見つけるようなものである複雑なアプリケーションを開発する際に非常に貴重な技術と考えられています。

## 参照
- デバッグに関する公式Oracleドキュメント: [Oracle Java SE デバッグ](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Eclipseのデバッグガイド: [Eclipse デバッグのヒント](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- 複数のコマンドラインJDKツールと軽量プロファイリング機能を統合した視覚的ツール、VisualVM: [VisualVM](https://visualvm.github.io/)
