---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:43.161824-07:00
description: "Vi\u1EC7c t\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0\
  \ vi\u1EC7c g\xF3i nh\u1EEFng \u0111o\u1EA1n m\xE3 nh\u1ECF l\u1EA1i \u0111\u1EC3\
  \ th\u1EF1c hi\u1EC7n nh\u1EEFng nhi\u1EC7m v\u1EE5 c\u1EE5 th\u1EC3. Ch\xFAng ta\
  \ l\xE0m \u0111i\u1EC1u n\xE0y b\u1EDFi v\xEC n\xF3 l\xE0m cho m\xE3 l\u1EC7nh\u2026"
lastmod: 2024-02-19 22:04:56.452572
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m l\xE0\
  \ vi\u1EC7c g\xF3i nh\u1EEFng \u0111o\u1EA1n m\xE3 nh\u1ECF l\u1EA1i \u0111\u1EC3\
  \ th\u1EF1c hi\u1EC7n nh\u1EEFng nhi\u1EC7m v\u1EE5 c\u1EE5 th\u1EC3. Ch\xFAng ta\
  \ l\xE0m \u0111i\u1EC1u n\xE0y b\u1EDFi v\xEC n\xF3 l\xE0m cho m\xE3 l\u1EC7nh\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 l\u1EADp tr\xECnh v\xE0o trong h\xE0m"
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Việc tổ chức mã lệnh thành các hàm là việc gói những đoạn mã nhỏ lại để thực hiện những nhiệm vụ cụ thể. Chúng ta làm điều này bởi vì nó làm cho mã lệnh dễ đọc, kiểm tra và sử dụng lại hơn - không ai muốn lội qua một đầm lầy của "mã spaghetti".

## Cách thực hiện:
Trong Fish, bạn viết một hàm với từ khóa `function`, đặt tên cho nó và kết thúc bằng `end`. Dưới đây là một ví dụ đơn giản:

```fish
function hello
    echo "Hello, World!"
end

hello
```

Kết quả:
```
Hello, World!
```

Bây giờ, hãy làm cho nó chào một người dùng:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

Kết quả:
```
Hey there, your_username!
```

Để lưu nó qua các phiên, sử dụng `funcsave greet`.

## Tìm hiểu sâu
Các hàm Fish Shell giống như các mini-script - bạn có thể chứa hầu như bất cứ điều gì vào đó. Từ lịch sử, khái niệm về các hàm trong viết kịch bản shell đã tiết kiệm vô số giờ gõ đi gõ lại và gỡ lỗi. Không giống như các ngôn ngữ lập trình như Python, các hàm Shell nhiều hơn về tiện ích hơn là cấu trúc.

Một số shell, như Bash, sử dụng `function` hoặc chỉ là các dấu ngoặc nhọn. Fish bám vào `function ... end` - rõ ràng và dễ đọc. Bên trong các hàm Fish, bạn có tất cả các tiện ích: tham số, biến cục bộ với `set -l`, và bạn thậm chí có thể định nghĩa một hàm bên trong một hàm khác.

Bạn sẽ không cần giá trị `return` vì Fish không tập trung vào đó; đầu ra của hàm là kết quả trả về của nó. Và nếu bạn muốn có các hàm cố định sẵn sàng cho các phiên trong tương lai, nhớ đến `funcsave`.

## Xem Thêm

- Hướng dẫn về hàm trong fish: [https://fishshell.com/docs/current/tutorial.html#tut_function](https://fishshell.com/docs/current/tutorial.html#functions)

### Các lệnh hàm

- [function](https://fishshell.com/docs/current/cmds/function.html) — Tạo một hàm
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — In hoặc xóa các hàm
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Lưu định nghĩa của một hàm vào thư mục tự động tải của người dùng
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Chỉnh sửa một hàm một cách tương tác
