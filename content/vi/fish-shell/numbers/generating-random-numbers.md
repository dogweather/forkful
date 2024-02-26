---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:26.517713-07:00
description: "Vi\u1EC7c sinh s\u1ED1 ng\u1EABu nhi\xEAn l\xE0 m\u1ED9t nhi\u1EC7m\
  \ v\u1EE5 c\u01A1 b\u1EA3n trong l\u1EADp tr\xECnh, \u0111\u01B0\u1EE3c s\u1EED\
  \ d\u1EE5ng cho m\u1ECDi th\u1EE9 t\u1EEB vi\u1EC7c l\u1EA5y m\u1EABu d\u1EEF li\u1EC7\
  u \u0111\u1EBFn ph\xE1t tri\u1EC3n tr\xF2 ch\u01A1i. Trong Fish\u2026"
lastmod: '2024-02-25T18:49:35.550117-07:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c sinh s\u1ED1 ng\u1EABu nhi\xEAn l\xE0 m\u1ED9t nhi\u1EC7m v\u1EE5\
  \ c\u01A1 b\u1EA3n trong l\u1EADp tr\xECnh, \u0111\u01B0\u1EE3c s\u1EED d\u1EE5\
  ng cho m\u1ECDi th\u1EE9 t\u1EEB vi\u1EC7c l\u1EA5y m\u1EABu d\u1EEF li\u1EC7u \u0111\
  \u1EBFn ph\xE1t tri\u1EC3n tr\xF2 ch\u01A1i. Trong Fish\u2026"
title: "Sinh s\u1ED1 ng\u1EABu nhi\xEAn"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc sinh số ngẫu nhiên là một nhiệm vụ cơ bản trong lập trình, được sử dụng cho mọi thứ từ việc lấy mẫu dữ liệu đến phát triển trò chơi. Trong Fish Shell, việc sử dụng các công cụ hệ thống và các hàm tích hợp sẵn cho mục đích này cho phép lập trình viên hiệu quả hơn khi kết hợp tính ngẫu nhiên và biến thiên vào trong script và ứng dụng.

## Cách thực hiện:

Việc tạo một số ngẫu nhiên trong Fish có thể được thực hiện một cách dễ dàng, sử dụng sự kết hợp của các tiện ích hệ thống và khả năng của shell. Dưới đây là một số ví dụ minh họa cách tạo ra số ngẫu nhiên trong các phạm vi chỉ định.

**Tạo một số ngẫu nhiên giữa 0 và 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Kết quả mẫu:**
```fish
42
```

**Tạo một số ngẫu nhiên giữa hai số bất kỳ, ví dụ từ 50 đến 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Kết quả mẫu:**
```fish
103
```

**Sử dụng random để trộn một danh sách:**

Bạn cũng có thể muốn trộn ngẫu nhiên các phần tử trong một danh sách. Đây là cách bạn có thể thực hiện:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Kết quả mẫu:**
```fish
C
A
E
D
B
```

Xin lưu ý, kết quả sẽ thay đổi mỗi lần bạn chạy những lệnh này do bản chất của tính ngẫu nhiên.

## Sâu hơn nữa

Hàm `random` của Fish Shell cung cấp một giao diện dễ sử dụng để sinh số giả ngẫu nhiên. Nội bộ, nó bao bọc xung quanh các tiện ích sinh số ngẫu nhiên ở cấp độ hệ thống, cung cấp một cách di động để đưa tính ngẫu nhiên vào script của bạn. Tuy nhiên, điều quan trọng cần nhớ là tính ngẫu nhiên được cung cấp bởi `random` đủ cho hầu hết các nhiệm vụ script nhưng có thể không đáp ứng được các yêu cầu bảo mật mật mã hóa cho các ứng dụng cần một mức độ không dự đoán cao hơn.

Đối với các bối cảnh bảo mật cao cấp, cân nhắc sử dụng các công cụ chuyên dụng hoặc thư viện lập trình được thiết kế cho mục đích mật mã hóa, chúng cung cấp các đảm bảo về tính ngẫu nhiên mạnh mẽ hơn. Tuy nhiên, cho các script chung và ứng dụng nơi mà các tiêu chuẩn bảo mật cao nhất cho tính ngẫu nhiên không phải là yêu cầu, hàm `random` của Fish Shell cung cấp một giải pháp tiện lợi và hiệu quả.
