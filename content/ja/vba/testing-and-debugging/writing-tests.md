---
title:                "テストの作成"
aliases: - /ja/vba/writing-tests.md
date:                  2024-02-01T22:09:06.226832-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/vba/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングでテストを書くとは、特定の手続きを作成してコードセグメントの機能性と性能を検証し、さまざまな条件下で期待通りに動作することを確認することを指します。プログラマーは、早期にバグを発見し、コード品質を向上させ、将来のコード保守と強化を容易にするためにこれを行います。

## 方法：

Visual Basic for Applications（VBA）には、PythonやJavaScriptのような言語で利用できるテストフレームワークと同様のものは組み込まれていませんが、コードの整合性をチェックするためのシンプルなテスト手順を実装することはできます。以下は例を示します：

2つの数値を加算するVBAの関数があるとします：

```basic
Function AddNumbers(x As Integer, y As Integer) As Integer
    AddNumbers = x + y
End Function
```

この関数をテストするには、出力を期待される結果と照らし合わせて検証する別の手続きを書くことができます：

```basic
Sub TestAddNumbers()
    Dim result As Integer
    result = AddNumbers(5, 10)
    If result = 15 Then
        MsgBox "Test Passed!", vbInformation
    Else
        MsgBox "Test Failed. Expected 15 but got " & result, vbCritical
    End If
End Sub
```

`TestAddNumbers`を実行すると、関数の出力に基づいてテストが合格したか失敗したかを示すメッセージボックスが表示されます。これは簡略化されたシナリオですが、ループを取り入れたり、異なる入力値を使ったり、複数の関数をテストしたりすることで、より複雑なテストを構築できます。

## 深堀り

ここで示されたVBAでのテストの書き方は手動であり、自動テスト実行、セットアップ/ティアダウン手順、テスト結果の統合報告など、他のプログラミング環境で利用可能なより洗練されたテストフレームワークの機能が欠けています。単体テストフレームワークやテスト駆動開発（TDD）の広範な採用以前は、説明されているような手動のテスト手順が一般的でした。この方法はシンプルで小規模なプロジェクトや学習目的には効果的かもしれませんが、大規模なプロジェクトやチームにはスケーラブルで効率的ではありません。

より豊富な開発ツールセットをサポートする環境では、プログラマーはしばしば、システマティックにテストを書いて実行するための包括的なツールを提供する、.NETアプリケーション用のNUnitやJavaアプリケーション用のJUnitのようなフレームワークに頼ります。これらのフレームワークは、テスト結果のアサーション、モックオブジェクトの設定、コードカバレッジの測定などの高度な機能を提供します。

より高度なテスト機能を求めるVBA開発者にとって、最も近い代替手段は、外部ツールを活用するか、他のプログラミング環境との統合かもしれません。一部の開発者は、VBAをExcelと組み合わせて、テストシナリオと結果を手動で記録します。専用のテストフレームワークを使用するほど便利で自動化されているわけではありませんが、これらの方法はVBAソリューションの信頼性を複雑または重要なアプリケーションで維持するのに役立つかもしれません。
