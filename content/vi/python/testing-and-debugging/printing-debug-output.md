---
aliases:
- /vi/python/printing-debug-output/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:45.932962-07:00
description: "In \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i gi\u1ED1ng nh\u01B0\
  \ vi\u1EC7c b\u1EA1n \u0111ang tr\xF2 chuy\u1EC7n v\u1EDBi m\xE3 c\u1EE7a m\xEC\
  nh \u0111\u1EC3 t\xECm hi\u1EC3u n\xF3 \u0111ang ngh\u0129 g\xEC. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo d\xF5i v\xE0 kh\u1EAF\
  c\u2026"
lastmod: 2024-02-18 23:08:50.270993
model: gpt-4-0125-preview
summary: "In \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i gi\u1ED1ng nh\u01B0 vi\u1EC7\
  c b\u1EA1n \u0111ang tr\xF2 chuy\u1EC7n v\u1EDBi m\xE3 c\u1EE7a m\xECnh \u0111\u1EC3\
  \ t\xECm hi\u1EC3u n\xF3 \u0111ang ngh\u0129 g\xEC. C\xE1c l\u1EADp tr\xECnh vi\xEA\
  n l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 theo d\xF5i v\xE0 kh\u1EAFc\u2026"
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
In đầu ra để gỡ lỗi giống như việc bạn đang trò chuyện với mã của mình để tìm hiểu nó đang nghĩ gì. Các lập trình viên làm điều này để theo dõi và khắc phục những lỗi nghiêm trọng xảy ra trong chương trình của họ.

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
