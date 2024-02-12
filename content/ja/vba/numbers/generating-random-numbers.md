---
title:                "ランダム数の生成"
aliases:
- /ja/vba/generating-random-numbers.md
date:                  2024-02-01T21:55:09.552052-07:00
model:                 gpt-4-0125-preview
simple_title:         "ランダム数の生成"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/generating-random-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
