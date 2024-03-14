---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:24.055079-07:00
description: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (stderr) g\u1EEDi c\xE1c th\xF4\
  ng b\xE1o l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n t\xE1ch r\u1EDDi v\u1EDBi k\u1EBF\
  t qu\u1EA3 ch\xEDnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\
  \u1EC3 b\xE1o c\xE1o v\u1EA5n \u0111\u1EC1 m\xE0 kh\xF4ng\u2026"
lastmod: '2024-03-13T22:44:37.232827-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (stderr) g\u1EEDi c\xE1c th\xF4\
  ng b\xE1o l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n t\xE1ch r\u1EDDi v\u1EDBi k\u1EBF\
  t qu\u1EA3 ch\xEDnh. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\
  \u1EC3 b\xE1o c\xE1o v\u1EA5n \u0111\u1EC1 m\xE0 kh\xF4ng\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc ghi vào lỗi chuẩn (stderr) gửi các thông báo lỗi và chẩn đoán tách rời với kết quả chính. Các lập trình viên sử dụng nó để báo cáo vấn đề mà không làm ảnh hưởng đến dòng dữ liệu thông thường.

## Làm thế nào:

Để ghi vào stderr trong Fish, sử dụng `echo` kèm theo `>&2`:

```Fish Shell
echo "Lỗi: Đã xảy ra sự cố" >&2
```

Kết quả sẽ không hiển thị trong đầu ra lệnh thông thường nhưng sẽ được hiển thị trên bảng điều khiển hoặc có thể được chuyển hướng sang một tệp:

```Fish Shell
echo "Lỗi: Đã xảy ra sự cố" >&2 > /dev/null
```

Lệnh này tắt âm thanh đầu ra chuẩn nhưng hiển thị thông báo lỗi.

## Sâu hơn

Ngay từ đầu, Unix đã thiết lập các luồng riêng biệt cho dữ liệu và lỗi: stdout và stderr. Việc tách chúng ra cho phép xử lý dữ liệu sạch sẽ và xử lý lỗi độc lập. Trong Fish, giống như trong các shell khác, `>&2` là một toán tử hướng đầu ra đến stderr. Các phương án thay thế để báo hiệu lỗi bao gồm trạng thái thoát và các cơ chế ghi log tùy chỉnh, nhưng việc ghi trực tiếp vào stderr là đơn giản và được sử dụng rộng rãi. Là một shell được thiết kế cho việc sử dụng tương tác, Fish kết hợp các đặc điểm từ các shell khác, bao gồm quy ước stderr này.

## Xem thêm

- Tài liệu Fish Shell: [Sử dụng stderr](https://fishshell.com/docs/current/index.html#redirection)
- Hướng dẫn lập kịch bản shell POSIX, áp dụng cho việc xử lý stderr: [Hướng dẫn Bash GNU](https://www.gnu.org/software/bash/manual/)
