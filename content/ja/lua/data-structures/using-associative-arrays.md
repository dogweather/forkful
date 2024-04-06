---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:07.803046-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Lua\u3067\u9023\u60F3\
  \u914D\u5217\uFF08\u3042\u308B\u3044\u306FLua\u7528\u8A9E\u3067\u306E\u30C6\u30FC\
  \u30D6\u30EB\uFF09\u3092\u4F5C\u6210\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\
  \u3002\u901A\u5E38\u306E\u6570\u5024\u30A4\u30F3\u30C7\u30C3\u30AF\u30B9\u3092\u6368\
  \u3066\u3066\u3001\u81EA\u5206\u3067\u9078\u3093\u3060\u30AD\u30FC\u3092\u4F7F\u3044\
  \u307E\u3059\u3002\u3053\u308C\u3092\u898B\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T21:53:43.142524-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## どのようにして：
Luaで連想配列（あるいはLua用語でのテーブル）を作成するのは簡単です。通常の数値インデックスを捨てて、自分で選んだキーを使います。これを見てください：

```Lua
-- 連想配列の作成
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- 要素へのアクセス
print(userInfo["name"]) -- Jamieを出力
print(userInfo.occupation) -- Adventurerを出力

-- 新しいキーと値の組み合わせの追加
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- 連想配列を繰り返し処理する
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

出力:
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

クールな部分？データをあなたにとって意味のあるキーを使って操作することで、コードをより読みやすく、保守しやすくなります。

## 深掘り
Luaが登場したとき、テーブルを万能データ構造として導入し、開発者がデータを管理する方法を革命的に変えました。一部の言語では連想配列と配列が別のエンティティであるのに対し、Luaのテーブルはそれらの両方の役割を果たし、データ構造の風景を単純化します。

Luaテーブルを特に強力にするのはその柔軟性です。しかし、この柔軟性は特に大きなデータセットで効率性のためにより専門化されたデータ構造が望ましい場合、潜在的なパフォーマンスの問題を招く可能性があります。

Luaはリンクリストやハッシュマップなど、より従来のデータ構造を標準でサポートしていませんが、テーブル構造の適応性はこれらをテーブルを使って実装する必要がある場合に可能にします。ただし、記憶しておくべきは、大いなる力には大いなる責任が伴います。柔軟性を賢く使って、コードのパフォーマンスと可読性を保持してください。
