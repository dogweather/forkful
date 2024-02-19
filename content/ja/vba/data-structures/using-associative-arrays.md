---
aliases:
- /ja/vba/using-associative-arrays/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:38.193716-07:00
description: "Visual Basic for Applications (VBA)\u2026"
lastmod: 2024-02-18 23:08:54.753745
model: gpt-4-0125-preview
summary: "Visual Basic for Applications (VBA)\u2026"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

Visual Basic for Applications (VBA) での連想配列は、しばしば辞書として知られており、プログラマーがキーと値のペアのコレクションを作成できるようにします。この機能は、効率的なデータの保存と取得に不可欠であり、伝統的な配列インデックスよりも柔軟で直感的なデータ管理方法を提供します。

## 方法：

VBAでは、`Dictionary`オブジェクトが連想配列に似た機能を提供します。これを使用するには、まずMicrosoft Scripting Runtimeに参照を追加する必要があります：

1. VBAエディターで、ツール > 参照へ行きます...
2. "Microsoft Scripting Runtime"をチェックしてOKをクリックします。

`Dictionary`の宣言、追加、およびアクセス方法は以下のとおりです：

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' アイテムの追加
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' アイテムのアクセス
Debug.Print sampleDictionary.Item("Name")  ' 出力：John Doe
Debug.Print sampleDictionary.Item("Age")   ' 出力：29

' キーが存在するか確認
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' アイテムの削除
sampleDictionary.Remove("Occupation")

' ディクショナリーをループする
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## ディープダイブ

`Dictionary`オブジェクトは、内部でWindows Scripting Hostのコンポーネントとインターフェースします。そのため、それは遅延バインドのCOMオブジェクトであり、過去にVBAの機能を拡張する一般的な方法でした。VBAでのその使用は、伝統的な配列やExcelの範囲に見られるような厳格な構造を強制することなく、複雑なデータセットを操作する言語の能力を大幅に向上させることができます。

心に留めておくべき一つの制限は、`Dictionary`にアクセスするためにはMicrosoft Scripting Runtimeへの参照の設定が必要であり、それがVBAプロジェクトの配布を複雑にする可能性があるということです。VBA内に存在するCollectionsのような代替品はありますが、エラーを引き起こすことなくキーの存在を簡単に確認できるといった`Dictionary`の重要な機能のいくつかを欠いています。

より最近のプログラミングの文脈では、Pythonのような言語では外部参照を追加する必要なしに連想配列（Pythonでも辞書として知られています）に対する組み込みサポートを提供しています。この組み込みサポートはプロセスを合理化し、箱から出してすぐにより高度な機能を提供します。しかし、VBAの範囲内およびMicrosoft Officeスイートのタスクを自動化する特定のアプリケーション向けには、`Dictionary`オブジェクトを使用することは連想配列のようなデータ構造に対して強力で関連性のある方法であり続けます。
