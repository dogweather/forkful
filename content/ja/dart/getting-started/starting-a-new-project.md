---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.773015-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.702607-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB"
---

{{< edit_this_page >}}

## 何となぜ？

Dartで新しいプロジェクトを始めることは、効率的な開発、テスト、そしてデプロイメントに適した環境を設定することを含みます。プログラマーは、特にFlutterのようなフレームワークを使用したウェブおよびモバイルアプリ開発において、Dartの最適なパフォーマンスと強固なエコシステムを活用するために新しいDartプロジェクトを開始します。

## 方法：

1. **Dartのインストール**：
   システムにDartがインストールされていることを確認します。そうでない場合は、[https://dart.dev/get-dart](https://dart.dev/get-dart) からダウンロードできます。インストールを以下のコマンドで確認してください：

   ```shell
   dart --version
   ```

2. **新しいDartプロジェクトを作成する**：
   Dart CLIを使って新しいプロジェクトを生成します：

   ```shell
   dart create hello_dart
   ```

   このコマンドは、選択に応じて、シンプルなサンプルウェブかコンソールアプリケーションを含む新しいディレクトリ `hello_dart` を作成します。

3. **プロジェクトの構造を調べる**：
   
   プロジェクトディレクトリに移動します：

   ```shell
   cd hello_dart
   ```

   典型的なDartプロジェクトには、以下の主要なファイルとディレクトリが含まれます：

   - `pubspec.yaml`：プロジェクトの依存関係とSDKの制約を含む設定ファイル。
   - `lib/`：ほとんどのDartコードが存在するディレクトリ。
   - `test/`：プロジェクトテストのためのディレクトリ。

4. **依存関係を追加する**：
   `pubspec.yaml` を編集して、依存関係を追加します。ウェブプロジェクトの場合は、HTTPリクエストを行うための人気パッケージ `http` を追加することを検討してください：

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   編集後、依存関係を取得します：

   ```shell
   dart pub get
   ```

5. **最初のDartコードを書く**：
   
   `lib/` ディレクトリ内に新しいDartファイル、`main.dart` を作成し、シンプルなDartコードを追加します：

   ```dart
   // Dartコアライブラリをインポートする
   import 'dart:core';

   void main() {
     print('Hello, Dart!');
   }
   ```

6. **Dartアプリケーションを実行する**：

   以下でDartプログラムを実行します：

   ```shell
   dart run
   ```

   出力は以下の通りです：

   ```
   Hello, Dart!
   ```

これらの手順に従って、Dartのインストールから最初のDartコードを実行するまで、新しいDartプロジェクトを成功させました。この基礎知識は、スケーラブルなアプリケーションの構築に対してDartの豊かなエコシステムとその機能を深く掘り下げるための土台となります。
