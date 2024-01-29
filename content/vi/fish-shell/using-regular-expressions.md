---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:57.921589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
