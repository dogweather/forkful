---
date: 2024-01-26 03:50:15.106700-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u306E\u30D0\u30B0\u3092\u30C6\u30B9\u30C8\u3057\u3066\u4FEE\
  \u6B63\u3059\u308B\u305F\u3081\u306E\u30C4\u30FC\u30EB\u3092\u4F7F\u3046\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u6D41\u308C\u3092\u7406\
  \u89E3\u3057\u3001\u30A8\u30E9\u30FC\u306E\u539F\u56E0\u3092\u7279\u5B9A\u3057\u3001\
  \u5B9F\u884C\u4E2D\u306E\u30ED\u30B8\u30C3\u30AF\u3092\u691C\u8A3C\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.953967-06:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u306E\u30D0\u30B0\u3092\u30C6\u30B9\u30C8\u3057\u3066\u4FEE\
  \u6B63\u3059\u308B\u305F\u3081\u306E\u30C4\u30FC\u30EB\u3092\u4F7F\u3046\u3053\u3068\
  \u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u6D41\u308C\u3092\u7406\
  \u89E3\u3057\u3001\u30A8\u30E9\u30FC\u306E\u539F\u56E0\u3092\u7279\u5B9A\u3057\u3001\
  \u5B9F\u884C\u4E2D\u306E\u30ED\u30B8\u30C3\u30AF\u3092\u691C\u8A3C\u3059\u308B\u305F\
  \u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用するとは、コードのバグをテストして修正するためのツールを使うことを意味します。プログラマーは、アプリケーションの流れを理解し、エラーの原因を特定し、実行中のロジックを検証するためにこれを行います。

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
