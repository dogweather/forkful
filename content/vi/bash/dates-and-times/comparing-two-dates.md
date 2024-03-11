---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:41.934650-07:00
description: "So s\xE1nh hai ng\xE0y cho ph\xE9p b\u1EA1n x\xE1c \u0111\u1ECBnh ng\xE0\
  y n\xE0o s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n, ho\u1EB7c n\u1EBFu ch\xFAng l\xE0\
  \ c\xF9ng m\u1ED9t th\u1EDDi \u0111i\u1EC3m. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 s\u1EAFp x\u1EBFp s\u1EF1\u2026"
lastmod: '2024-03-11T00:14:10.186780-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y cho ph\xE9p b\u1EA1n x\xE1c \u0111\u1ECBnh ng\xE0\
  y n\xE0o s\u1EDBm h\u01A1n, mu\u1ED9n h\u01A1n, ho\u1EB7c n\u1EBFu ch\xFAng l\xE0\
  \ c\xF9ng m\u1ED9t th\u1EDDi \u0111i\u1EC3m. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 s\u1EAFp x\u1EBFp s\u1EF1\u2026"
title: "So s\xE1nh hai ng\xE0y"
---

{{< edit_this_page >}}

## Lý do và Ý nghĩa
So sánh hai ngày cho phép bạn xác định ngày nào sớm hơn, muộn hơn, hoặc nếu chúng là cùng một thời điểm. Các lập trình viên làm điều này để sắp xếp sự kiện, kích hoạt các hành động theo thời gian, hoặc chỉ đơn giản là theo dõi thời gian đã qua.

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
