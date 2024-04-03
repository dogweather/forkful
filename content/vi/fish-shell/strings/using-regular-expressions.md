---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:57.921589-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, hay c\xF2n g\u1ECDi l\xE0 regex, l\xE0\
  \ c\xE1c m\u1EABu m\xF4 t\u1EA3 c\xE1c t\u1EADp h\u1EE3p chu\u1ED7i. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng \u0111\u1EC3 t\xECm ki\u1EBFm, kh\u1EDBp,\
  \ v\xE0 thao t\xE1c v\u1EDBi v\u0103n b\u1EA3n \u2014\u2026"
lastmod: '2024-03-13T22:44:37.194748-06:00'
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy, hay c\xF2n g\u1ECDi l\xE0 regex, l\xE0\
  \ c\xE1c m\u1EABu m\xF4 t\u1EA3 c\xE1c t\u1EADp h\u1EE3p chu\u1ED7i."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

## Cái Gì & Tại Sao?
Biểu thức chính quy, hay còn gọi là regex, là các mẫu mô tả các tập hợp chuỗi. Lập trình viên sử dụng chúng để tìm kiếm, khớp, và thao tác với văn bản — cực kỳ tiện lợi cho việc tìm kiếm "kim trong đống rơm" dữ liệu.

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
