---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:21.447320-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh l\xE0 vi\u1EC7\
  c l\u1EA5y nh\u1EEFng ph\u1EA7n th\xEAm b\u1EA1n g\xF5 sau t\xEAn c\u1EE7a script,\
  \ gi\u1ED1ng nh\u01B0 nh\u1EEFng c\xE1i b\u1EAFt tay b\xED m\u1EADt \u0111\u1EC3\
  \ t\xF9y ch\u1EC9nh h\xE0nh vi c\u1EE7a script. L\u1EADp\u2026"
lastmod: '2024-03-13T22:44:37.231479-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh l\xE0 vi\u1EC7\
  c l\u1EA5y nh\u1EEFng ph\u1EA7n th\xEAm b\u1EA1n g\xF5 sau t\xEAn c\u1EE7a script,\
  \ gi\u1ED1ng nh\u01B0 nh\u1EEFng c\xE1i b\u1EAFt tay b\xED m\u1EADt \u0111\u1EC3\
  \ t\xF9y ch\u1EC9nh h\xE0nh vi c\u1EE7a script. L\u1EADp\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Đọc các đối số dòng lệnh là việc lấy những phần thêm bạn gõ sau tên của script, giống như những cái bắt tay bí mật để tùy chỉnh hành vi của script. Lập trình viên làm điều này để làm cho script linh hoạt và tương tác mà không cần rắc rối.

## Làm thế nào:

Giả sử `greet.fish` là script của bạn. Bạn muốn nó nhận một tên và phát ra lời chào.

```fish
#!/usr/bin/env fish

# Các đối số được lưu trong $argv
# $argv[1] là đối số đầu tiên, $argv[2] là đối số thứ hai, và cứ thế.

set name $argv[1]
echo "Xin chào, $name!"
```

Chạy nó:

```shell
$ fish greet.fish World
Xin chào, World!
```

Bây giờ, với nhiều đối số:

```fish
#!/usr/bin/env fish

# Lặp qua tất cả các đối số
for arg in $argv
    echo "Xin chào, $arg!"
end
```

Thử nó:

```shell
$ fish greet.fish Earth Mars Venus
Xin chào, Earth!
Xin chào, Mars!
Xin chào, Venus!
```

Để xử lý các cờ (như `-u` cho chữ hoa):

```fish
#!/usr/bin/env fish

# Kiểm tra đối số "-u"
set -l uppercase_mode tắt
for arg in $argv
    if test "$arg" = "-u"
        set uppercase_mode bật
    else if set -q uppercase_mode[1]; and string match --quiet -- "$uppercase_mode" "bật"
        echo (string upper "$arg")
    else
        echo $arg
    end
end
```

Và kích hoạt:

```shell
$ fish greet.fish -u mercury venus
MERCURY
VENUS
```

## Sâu hơn

Fish Shell đã nắm rõ việc đọc các đối số dòng lệnh từ lâu, giống như các shells khác. Điều đặc biệt ở Fish là sự đơn giản trong thiết kế. Không có `$1, $2... $n` để nhớ; nó là một mảng `$argv`, một lãnh thổ quen thuộc nếu bạn thích thú với những ngôn ngữ lập trình khác.

Có những lựa chọn thay thế, như bash, zsh, v.v., nhưng cú pháp script của Fish nhằm mục đích đọc và viết dễ dàng hơn. Thay vì sử dụng các lệnh `shift` truyền thống hoặc xử lý `$@` cho tất cả các đối số, Fish có `$argv` thân thiện đó và những cấu trúc script đáng yêu như vòng lặp `for` và điều kiện `if` ít về những biểu tượng khó hiểu mà nhiều về những từ ngữ rõ ràng.

Khi thực hiện, điều quan trọng là cần xem xét cách script của bạn sẽ được sử dụng. Nó có cần giá trị mặc định không? Người dùng có biết phải nhập gì không? Đảm bảo rằng bạn xử lý các trường hợp mà người dùng quên không truyền đối số hoặc truyền chúng không đúng thứ tự.

## Xem Thêm

- Tài liệu chính thức về đối số dòng lệnh của Fish: [fishshell.com/docs/current/#syntax-command-line](https://fishshell.com/docs/current/#syntax-command-line)
- Đối với kịch bản nâng cao và tạo các hàm của riêng bạn trong Fish: [fishshell.com/docs/current/#defining-functions](https://fishshell.com/docs/current/#defining-functions)
- Một giới thiệu về Fish cho người dùng có nền tảng về các shell khác: [fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
