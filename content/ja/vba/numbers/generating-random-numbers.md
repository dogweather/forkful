---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:09.552052-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A VBA\u3067\u306F\u3001`Rnd`\u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u4E71\u6570\u3092\u751F\u6210\u3057\u307E\u3059\
  \u3002\u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306F\u3001`Rnd`\u306F0\u4EE5\u4E0A1\u672A\
  \u6E80\u306E\u5358\u7CBE\u5EA6\u6D6E\u52D5\u5C0F\u6570\u70B9\u6570\u3092\u751F\u6210\
  \u3057\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u52B9\u679C\u7684\u306B\u4E71\u6570\
  \u3092\u6D3B\u7528\u3059\u308B\u305F\u3081\u306E\u3044\u304F\u3064\u304B\u306E\u30B9\
  \u30C6\u30C3\u30D7\u3068\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A 1. **\u5358\
  \u7D14\u306A\u4E71\u6570\uFF1A** \u57FA\u672C\u7684\u306A\u4E71\u6570\u3092\u751F\
  \u6210\u3059\u308B\u306B\u306F\u3001`Rnd()`\u3092\u547C\u3073\u51FA\u3059\u3060\u3051\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.157393-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\uFF1A VBA\u3067\u306F\u3001`Rnd`\u95A2\u6570\
  \u3092\u4F7F\u7528\u3057\u3066\u4E71\u6570\u3092\u751F\u6210\u3057\u307E\u3059\u3002\
  \u30C7\u30D5\u30A9\u30EB\u30C8\u3067\u306F\u3001`Rnd`\u306F0\u4EE5\u4E0A1\u672A\u6E80\
  \u306E\u5358\u7CBE\u5EA6\u6D6E\u52D5\u5C0F\u6570\u70B9\u6570\u3092\u751F\u6210\u3057\
  \u307E\u3059\u3002\u3053\u3053\u3067\u306F\u52B9\u679C\u7684\u306B\u4E71\u6570\u3092\
  \u6D3B\u7528\u3059\u308B\u305F\u3081\u306E\u3044\u304F\u3064\u304B\u306E\u30B9\u30C6\
  \u30C3\u30D7\u3068\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A 1. **\u5358\u7D14\
  \u306A\u4E71\u6570\uFF1A** \u57FA\u672C\u7684\u306A\u4E71\u6570\u3092\u751F\u6210\
  \u3059\u308B\u306B\u306F\u3001`Rnd()`\u3092\u547C\u3073\u51FA\u3059\u3060\u3051\u3067\
  \u3059\uFF1A."
title: "\u30E9\u30F3\u30C0\u30E0\u6570\u306E\u751F\u6210"
weight: 12
---

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
