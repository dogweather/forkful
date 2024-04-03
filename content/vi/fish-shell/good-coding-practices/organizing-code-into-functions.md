---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:01:43.161824-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Fish, b\u1EA1n vi\u1EBFt m\u1ED9\
  t h\xE0m v\u1EDBi t\u1EEB kh\xF3a `function`, \u0111\u1EB7t t\xEAn cho n\xF3 v\xE0\
  \ k\u1EBFt th\xFAc b\u1EB1ng `end`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED\
  \ d\u1EE5 \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:37.218141-06:00'
model: gpt-4-0125-preview
summary: "Trong Fish, b\u1EA1n vi\u1EBFt m\u1ED9t h\xE0m v\u1EDBi t\u1EEB kh\xF3a\
  \ `function`, \u0111\u1EB7t t\xEAn cho n\xF3 v\xE0 k\u1EBFt th\xFAc b\u1EB1ng `end`."
title: "S\u1EAFp x\u1EBFp m\xE3 l\u1EADp tr\xECnh v\xE0o trong h\xE0m"
weight: 18
---

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
