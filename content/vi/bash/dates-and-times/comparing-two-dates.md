---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:41.934650-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t c\xE1ch nhanh ch\xF3ng \u0111\u1EC3 so s\xE1nh hai ng\xE0y trong Bash."
lastmod: '2024-03-13T22:44:36.895288-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t c\xE1ch nhanh ch\xF3ng \u0111\u1EC3\
  \ so s\xE1nh hai ng\xE0y trong Bash."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
Dưới đây là một cách nhanh chóng để so sánh hai ngày trong Bash:

```Bash
ngay1="2023-04-01"
ngay2="2023-04-15"

# Chuyển đổi các ngày thành số giây kể từ thời điểm đầu (epoch)
giay1=$(date -d "$ngay1" +%s)
giay2=$(date -d "$ngay2" +%s)

# So sánh các ngày
if [ $giay1 -eq $giay2 ]; then
    echo "Các ngày giống nhau."
elif [ $giay1 -lt $giay2 ]; then
    echo "Ngày $ngay1 sớm hơn $ngay2."
else
    echo "Ngày $ngay1 muộn hơn $ngay2."
fi
```

Kết quả mẫu nếu `$ngay2` muộn hơn:

```
Ngày 2023-04-01 sớm hơn 2023-04-15.
```

## Khám phá sâu hơn
Trong lịch sử, việc so sánh các ngày trong kịch bản shell không phải là điều dễ dàng do sự khác biệt về định dạng ngày và thiếu các hàm được xây dựng sẵn. Lệnh `date`, với `%s` để chuyển đổi các ngày thành giây kể từ thời điểm Unix epoch (00:00:00 UTC ngày 1 tháng 1 năm 1970), thực sự là một vị cứu tinh.

Các phương pháp thay thế bao gồm sử dụng các công cụ bên ngoài như `awk` hoặc so sánh chuỗi - rủi ro nếu định dạng thay đổi. Về mặt triển khai, một điều kỳ quặc là xử lý các múi giờ: thêm `TZ=UTC` trước các lệnh `date` đảm bảo so sánh UTC.

Toán học với ngày, như việc tìm sự khác biệt giữa các ngày, có thể trở nên phức tạp. Thêm hoặc bớt ngày đòi hỏi nhiều thủ thuật với lệnh `date` hơn. Các trường hợp ngoại lệ, như giây nhuận hoặc sự chuyển đổi giờ tiết kiệm ánh sáng, có thể gây ra lỗi.

## Xem thêm
- [Trang hướng dẫn `date`](https://man7.org/linux/man-pages/man1/date.1.html) cho các tùy chọn định dạng.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/bash) cho kiến thức cộng đồng và khắc phục sự cố.
