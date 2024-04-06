---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:22.285031-07:00
description: "\u65B9\u6CD5\uFF1A Ruby\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u30C1\u30A7\
  \u30C3\u30AF\u3059\u308B\u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u306F\u7D14\u7C8B\u306ARuby\u3060\
  \u3051\u3092\u4F7F\u3063\u3066\u305D\u308C\u3092\u884C\u3046\u65B9\u6CD5\u3067\u3059\
  \u3001\u4F55\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3082\u5FC5\u8981\u3042\u308A\u307E\u305B\u3093\uFF1A."
lastmod: '2024-04-05T22:38:42.359113-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Ruby\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\
  \u3001\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u30C1\u30A7\u30C3\
  \u30AF\u3059\u308B\u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u306F\u7D14\u7C8B\u306ARuby\u3060\
  \u3051\u3092\u4F7F\u3063\u3066\u305D\u308C\u3092\u884C\u3046\u65B9\u6CD5\u3067\u3059\
  \u3001\u4F55\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3082\u5FC5\u8981\u3042\u308A\u307E\u305B\u3093\uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
Rubyの標準ライブラリは、ディレクトリの存在をチェックするための直接的な方法を提供します。こちらは純粋なRubyだけを使ってそれを行う方法です、何のサードパーティライブラリも必要ありません：

```ruby
require 'fileutils'

# ディレクトリが存在するか確認
if Dir.exist?('/path/to/directory')
  puts 'ディレクトリが存在します。'
else
  puts 'ディレクトリは存在しません。'
end
```
サンプル出力：
```
ディレクトリが存在します。
```
または：
```
ディレクトリは存在しません。
```

`Dir.exist?`を使う以外にも、与えられたパスがディレクトリである場合`true`を返す`File.directory?`メソッドを利用することもできます：

```ruby
if File.directory?('/path/to/directory')
  puts 'ディレクトリが存在します。'
else
  puts 'ディレクトリは存在しません。'
end
```
`Dir.exist?`と`File.directory?`の両方はRubyの標準ライブラリの一部であり、使用するために外部のgemsを必要とせず、ディレクトリのチェックに便利かつ効率的なオプションとなっています。
