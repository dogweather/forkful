---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:02:01.685275-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/fish-shell/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Xử lý lỗi cho phép kịch bản của bạn giải quyết những tình huống không mong đợi một cách nhẹ nhàng. Chúng ta làm điều này để quản lý sự cố mà không làm người dùng của chúng ta phải nhức đầu.

## Cách thực hiện:
Để bắt lỗi trong Fish, hãy dựa vào lệnh `status` và các điều kiện. Giả sử `ping` thất bại; đây là cách để phát hiện điều đó:

```fish
ping -c 1 example.com
if not status is-success
    echo "Có điều gì đó không ổn với ping."
end
```

Kết quả mẫu nếu `ping` thất bại:

```
Có điều gì đó không ổn với ping.
```

Để xử lý một mã lỗi cụ thể, sử dụng `status --is`:

```fish
false
if status --is 1
    echo "Đã bắt được một lỗi với mã 1."
end
```

Kết quả mẫu:
```
Đã bắt được một lỗi với mã 1.
```

Để có cách tiếp cận mạnh mẽ hơn, hãy cân nhắc sử dụng một hàm:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping thất bại với trạng thái $status"
        return 1
    end
end

try_ping
```

## Sâu hơn nữa
Xử lý lỗi trong Fish không giống với mô hình `try/catch` mà bạn có thể biết từ các ngôn ngữ cấp cao hơn. Thay vào đó, bạn có các trạng thái thoát ra đơn giản được cung cấp bởi lệnh `status`.

Về mặt lịch sử, trong các hệ thống giống Unix, một trạng thái thoát ra là `0` có nghĩa là thành công, trong khi bất kỳ giá trị không phải không nào cũng chỉ ra một lỗi, thường phản ánh các lý do thất bại khác nhau. Quy ước này được sử dụng bởi hầu hết các tiện ích dòng lệnh và do đó, bởi chính Fish.

Các phương án thay thế cho các kiểm tra `status` trong Fish bao gồm xử lý tín hiệu thông qua `trap` trong các shell khác, nhưng Fish ưa thích kiểm tra trạng thái rõ ràng hơn, vì nó sạch sẽ hơn và ít gây ra tác dụng phụ.

Về mặt thực hiện, xử lý lỗi trong Fish vẫn đơn giản nhưng mạnh mẽ, chủ yếu nhờ vào bản chất không chặn của nó và nhấn mạnh vào cú pháp rõ ràng, như được hiển thị trong các ví dụ. Mã lỗi kết hợp đẹp mắt với các hàm, cho phép quản lý lỗi một cách có mô-đun và dễ đọc.

## Tham khảo thêm
- Tài liệu về điều kiện trong Fish: https://fishshell.com/docs/current/language.html#conditionals
- Hướng dẫn về xử lý lỗi trong Fish: https://fishshell.com/docs/current/tutorial.html#error-handling
