---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:57.921589-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Fish Shell c\xF3 h\u1ED7 tr\u1EE3 regex t\xED\
  ch h\u1EE3p trong c\xE1c l\u1EC7nh nh\u01B0 `string`. H\xE3y c\xF9ng t\xECm hi\u1EC3\
  u qua m\u1ED9t s\u1ED1 v\xED d\u1EE5: **T\xECm Ki\u1EBFm C\u01A1 B\u1EA3n:** T\xEC\
  m xem t\u1EEB \"fish\"\u2026"
lastmod: '2024-03-13T22:44:37.194748-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell c\xF3 h\u1ED7 tr\u1EE3 regex t\xEDch h\u1EE3p trong c\xE1c l\u1EC7\
  nh nh\u01B0 `string`."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Làm Thế Nào:
Fish Shell có hỗ trợ regex tích hợp trong các lệnh như `string`. Hãy cùng tìm hiểu qua một số ví dụ:

**Tìm Kiếm Cơ Bản:**

Tìm xem từ "fish" có trong chuỗi không:

```fish
echo "I love to fish for fish in my fish tank" | string match -r "fish"
```

Kết quả:

```
fish
fish
fish
```

**Nhóm Bắt Giữ:**

Trích xuất các nhóm phù hợp sử dụng dấu ngoặc đơn:

```fish
echo "Color: Blue, Code: #0000FF" | string match -r "Color: (\w+)"
```

Kết quả:

```
Color: Blue
Blue
```

**Thay Thế Văn Bản:**

Đổi "fish" thành "shark":

```fish
echo "One fish, two fish, red fish, blue fish" | string replace -ar "fish" "shark"
```

Kết quả:

```
One shark, two shark, red shark, blue shark
```

## Đào Sâu Hơn:
Biểu thức chính quy có nguồn gốc từ khoa học máy tính lý thuyết, được tạo ra trong những năm 1950. Có phương án thay thế? Chắc chắn, bạn có thể sử dụng tìm kiếm chuỗi đơn giản hoặc bộ phân tích cú pháp để cấu trúc hơn, nhưng regex là giải pháp ngọt ngào cho những nhiệm vụ nhanh chóng và bẩn thỉu. Fish Shell sử dụng PCRE (Perl Compatible Regular Expressions) ở trong, đảm bảo một bộ tính năng mạnh mẽ cho việc khớp mẫu.

## Xem Thêm:
- Tài liệu chính thức của Fish Shell: [Lệnh string](https://fishshell.com/docs/current/cmds/string.html)
- Hướng dẫn Regex cho người mới bắt đầu: [Regular Expressions 101](https://regex101.com/)
- Hiểu sâu hơn: [Mastering Regular Expressions của Jeffrey Friedl](http://shop.oreilly.com/product/9780596528126.do)
