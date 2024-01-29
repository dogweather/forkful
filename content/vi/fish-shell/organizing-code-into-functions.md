---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:19.669689-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Việc tổ chức mã lệnh thành các hàm chính là việc gom các đoạn mã script để thực hiện các nhiệm vụ cụ thể. Chúng ta làm điều này vì nó giúp mã lệnh dễ đọc, dễ kiểm tra và tái sử dụng hơn — không ai muốn lội qua một bãi lầy của mã spaghetti.

## Cách thực hiện:
Trong Fish, bạn viết một hàm với từ khóa `function`, đặt cho nó một tên và kết thúc với `end`. Dưới đây là một ví dụ đơn giản:

```fish
function hello
    echo "Chào, Thế giới!"
end

hello
```

Kết quả:
```
Chào, Thế giới!
```

Bây giờ, hãy tạo nó để chào một người dùng:

```fish
function greet
    set user (whoami)
    echo "Chào bạn, $user!"
end

greet
```

Kết quả:
```
Chào bạn, your_username!
```

Để lưu nó qua các phiên, sử dụng `funcsave greet`.

## Sâu hơn nữa
Các hàm trong Fish Shell giống như các mini-scripts — bạn có thể nhét hầu như bất kỳ thứ gì vào đó. Từ trước đến nay, khái niệm về hàm trong mã lệnh shell đã giúp tiết kiệm vô số giờ gõ lại lặp đi lặp lại và gỡ lỗi. Không giống như các ngôn ngữ lập trình như Python, hàm Shell nhiều về sự tiện lợi hơn là cấu trúc.

Một số shell, như Bash, sử dụng `function` hoặc chỉ là ngoặc nhọn. Fish tuân theo `function ... end`— rõ ràng và dễ đọc. Bên trong các hàm Fish, bạn có tất cả các tiện ích: tham số, biến cục bộ với `set -l`, và bạn thậm chí còn có thể định nghĩa một hàm bên trong một hàm khác.

Bạn sẽ không cần một giá trị `return` vì Fish không chú trọng vào điều đó; output của hàm chính là giá trị trả về của nó. Và nếu bạn muốn những hàm tồn tại liên tục cho các phiên tương lai, hãy nhớ đến `funcsave`.

## Xem thêm
- Hướng dẫn về hàm trong fish: https://fishshell.com/docs/current/tutorial.html#tut_functions
- Tài liệu fish cho `function`: https://fishshell.com/docs/current/cmds/function.html
- Hướng dẫn chi tiết viết hàm trong fish: https://fishshell.com/docs/current/index.html#syntax-function
