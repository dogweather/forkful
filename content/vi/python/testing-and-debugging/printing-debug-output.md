---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:45.932962-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: \u0110\u01A1n gi\u1EA3n v\xE0 d\u1EC5 hi\u1EC3\
  u, b\u1EA1n in nh\u1EEFng th\u1EE9 ra \u0111\u1EC3 xem \u0111i\u1EC1u g\xEC \u0111\
  ang di\u1EC5n ra. D\u01B0\u1EDBi \u0111\xE2y l\xE0 \u0111i\u1EC3n h\xECnh."
lastmod: '2024-03-13T22:44:36.100502-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u01A1n gi\u1EA3n v\xE0 d\u1EC5 hi\u1EC3u, b\u1EA1n in nh\u1EEFng\
  \ th\u1EE9 ra \u0111\u1EC3 xem \u0111i\u1EC1u g\xEC \u0111ang di\u1EC5n ra."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cách thực hiện:
Đơn giản và dễ hiểu, bạn in những thứ ra để xem điều gì đang diễn ra. Dưới đây là điển hình:

```Python
print("Xin chào, các thợ săn lỗi!")
```

Bạn đã cảm thấy mình như một thám tử chưa? Bây giờ, hãy xem các biến của bạn đang hoạt động như thế nào:

```Python
buggy_number = 42
print(f"Debug: Số này là {buggy_number}")
```

Khi mọi thứ trở nên phức tạp, bạn có thể muốn nhìn vào một danh sách:

```Python
buggy_list = [1, 2, 3]
print(f"Debug: Danh sách chứa {buggy_list}")
```

Chạy những đoạn mã này, và đầu ra của bạn là:

```
Xin chào, các thợ săn lỗi!
Debug: Số này là 42
Debug: Danh sách chứa [1, 2, 3]
```

## Sâu hơn
Gỡ lỗi bằng cách in ra có một lịch sử lâu dài, kéo dài từ thời kỳ khủng long đi dạo trên mặt đất (còn được biết đến là những ngày đầu của ngành máy tính). Nó đơn giản và áp dụng được mọi nơi vì chỉ cần in ra bất cứ thứ gì bạn muốn kiểm tra.

Trong khi `print()` là công cụ nhanh và dễ dàng trong Python, các lựa chọn khác cũng tồn tại. Đối với việc điều tra kỹ lưỡng hơn, bạn có thể muốn sử dụng logging với các mức độ khác nhau như DEBUG, INFO, WARNING, v.v. Như thế, bạn có thể kiểm soát những gì được in ra và những gì được giữ lặng.

Đôi khi, bạn sẽ nghe về những trình gỡ lỗi sang trọng cho phép bạn tạm dừng thời gian (một cách nào đó) và soi xem mã của mình đang chạy như thế nào. Chúng rất mạnh mẽ và đáng để học, nhưng đừng để chúng làm bạn cảm thấy tồi tệ vì đã thêm một lệnh `print()` nhanh chóng ở đây và ở đó.

Về việc triển khai, sự đơn giản của `print()` chính là vẻ đẹp của nó. Hãy nhớ rằng, việc liên tục in ra console có thể làm chậm bạn lại nếu bạn làm việc đó hàng triệu lần trong một vòng lặp. Và, nó có thể trở nên lộn xộn nhanh chóng. Hãy ghi chú hoặc loại bỏ những dòng đó một khi bạn đã giải quyết xong những lỗi.

## Xem Thêm
Để biết thêm về việc in và gỡ lỗi trong Python:
- Hàm `print()` được tích hợp sẵn của Python: [Tài liệu Python về print](https://docs.python.org/3/library/functions.html#print)
- Python Logging: [Hướng dẫn Logging](https://docs.python.org/3/howto/logging.html)
- Dành cho những ai yêu thích trình gỡ lỗi: [Tài liệu Python về pdb](https://docs.python.org/3/library/pdb.html)
