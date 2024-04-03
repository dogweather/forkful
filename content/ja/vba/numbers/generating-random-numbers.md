---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:09.552052-07:00
description: "Visual Basic for\u2026"
lastmod: '2024-03-13T22:44:41.878864-06:00'
model: gpt-4-0125-preview
summary: "Visual Basic for Applications\uFF08VBA\uFF09\u3067\u4E71\u6570\u3092\u751F\
  \u6210\u3059\u308B\u3053\u3068\u3067\u3001\u30B5\u30A4\u30B3\u30ED\u306E\u30ED\u30FC\
  \u30EB\u3084\u30C7\u30FC\u30BF\u306E\u30B5\u30F3\u30D7\u30EA\u30F3\u30B0\u306A\u3069\
  \u3001\u5076\u7136\u3084\u5909\u52D5\u6027\u306E\u8981\u7D20\u3092\u4F34\u3046\u30D7\
  \u30ED\u30BB\u30B9\u3092\u30B7\u30DF\u30E5\u30EC\u30FC\u30C8\u3059\u308B\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u6E2C\u53EF\u80FD\u306A\u7D50\u679C\u304C\
  \u975E\u73FE\u5B9F\u7684\u307E\u305F\u306F\u3042\u307E\u308A\u5F79\u306B\u7ACB\u305F\
  \u306A\u3044\u30E2\u30C7\u30EB\u3001\u30B2\u30FC\u30E0\u3001\u307E\u305F\u306F\u30B7\
  \u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3092\u958B\u767A\u3059\u308B\u305F\u3081\
  \u306B\u3001\u3053\u308C\u3089\u306E\u6280\u8853\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \u3002."
title: "\u30E9\u30F3\u30C0\u30E0\u6570\u306E\u751F\u6210"
weight: 12
---

## 何となぜ？

Visual Basic for Applications（VBA）で乱数を生成することで、サイコロのロールやデータのサンプリングなど、偶然や変動性の要素を伴うプロセスをシミュレートするプログラムが可能になります。プログラマーは、予測可能な結果が非現実的またはあまり役に立たないモデル、ゲーム、またはシミュレーションを開発するために、これらの技術を使用します。

## どのように：

VBAでは、`Rnd`関数を使用して乱数を生成します。デフォルトでは、`Rnd`は0以上1未満の単精度浮動小数点数を生成します。ここでは効果的に乱数を活用するためのいくつかのステップと例を紹介します：

1. **単純な乱数：**
   基本的な乱数を生成するには、`Rnd()`を呼び出すだけです：

   ```vb
   Sub GenerateRandomNumber()
       Dim randomNumber As Single
       randomNumber = Rnd() ' 0と1の間の乱数
       MsgBox randomNumber
   End Sub
   ```

2. **シードの設定：**
   `Randomize`ステートメントは乱数生成器を初期化し、VBAコードが実行されるたびに異なる結果を確実にするために重要です：

   ```vb
   Sub SeedRandomNumber()
       Randomize
       Dim randomNumber As Single
       randomNumber = Rnd()
       MsgBox randomNumber
   End Sub
   ```

3. **範囲内の数値を生成する：**
   よく、特定の範囲内の乱数が欲しい場合があります。1から100の間の数値を生成する方法は次のとおりです：

   ```vb
   Sub RandomNumberInRange()
       Randomize
       Dim randomNumber As Integer
       randomNumber = Int((100 * Rnd()) + 1) ' 1から100の間の乱数
       MsgBox randomNumber
   End Sub
   ```

### サンプル出力：
`RandomNumberInRange`を実行した後、メッセージボックスには`45`のような数値が表示されるかもしれません。

## 深掘り：

VBAの`Rnd`関数は使用が簡単ですが、実際には決定論的アルゴリズムに基づいて擬似乱数を生成します。これは、生成される数値のシーケンスが本当にランダムではないが、確率プロセスを必要とする一般的なタスクでしばしば十分であることを意味します。

歴史的に、VBAの乱数生成能力はBasicの初期バージョンにさかのぼり、アルゴリズムに開始点をシードする`Randomize`のような機能を含むように時間とともに適応してきました。しかし、セキュアな暗号操作など、高いランダム性を要求するアプリケーションには、VBAの`Rnd`は最適なツールではないかもしれません。Pythonの`secrets`モジュールやJavaの`SecureRandom`のような、暗号学を念頭に置いて設計されたより堅牢なプログラミング環境や言語での代替案を検討するべきです。

その限界にもかかわらず、VBAで乱数を生成する簡便性とアクセスの容易さは、幅広い軽量アプリケーション、シミュレーション作業、教育目的にとって貴重なツールであることを続けています。
