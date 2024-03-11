---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:50.571678-07:00
description: "T\xE1i c\u1EA5u tr\xFAc trong Dart l\xE0 qu\xE1 tr\xECnh c\u01A1 c\u1EA5\
  u l\u1EA1i code hi\u1EC7n t\u1EA1i m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi\
  \ b\xEAn ngo\xE0i c\u1EE7a n\xF3, nh\u1EB1m m\u1EE5c \u0111\xEDch c\u1EA3i thi\u1EC7\
  n c\u1EA5u tr\xFAc n\u1ED9i b\u1ED9, kh\u1EA3\u2026"
lastmod: '2024-03-11T00:14:09.528456-06:00'
model: gpt-4-0125-preview
summary: "T\xE1i c\u1EA5u tr\xFAc trong Dart l\xE0 qu\xE1 tr\xECnh c\u01A1 c\u1EA5\
  u l\u1EA1i code hi\u1EC7n t\u1EA1i m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi\
  \ b\xEAn ngo\xE0i c\u1EE7a n\xF3, nh\u1EB1m m\u1EE5c \u0111\xEDch c\u1EA3i thi\u1EC7\
  n c\u1EA5u tr\xFAc n\u1ED9i b\u1ED9, kh\u1EA3\u2026"
title: "T\xE1i c\u1EA5u tr\xFAc code"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tái cấu trúc trong Dart là quá trình cơ cấu lại code hiện tại mà không thay đổi hành vi bên ngoài của nó, nhằm mục đích cải thiện cấu trúc nội bộ, khả năng đọc và bảo trì của nó. Các lập trình viên thường tái cấu trúc để làm cho code sạch sẽ hơn, dễ hiểu hơn hoặc hiệu quả hơn, tạo điều kiện cho việc chỉnh sửa tương lai dễ dàng hơn và giảm khả năng xuất hiện lỗi.

## Làm thế nào:

### Ví dụ 1: Đổi tên và Trích xuất Phương thức

Trước khi tái cấu trúc, bạn có thể có một đoạn mã mà trộn lẫn nhiều cấp độ trừu tượng hoặc trách nhiệm, như tính một khoản giảm giá và sau đó áp dụng nó:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Giá cuối: $finalPrice");
}
```

**Đầu ra:**
```
Giá cuối: 80.0
```

Sau khi tái cấu trúc, bạn có thể trích xuất tính toán giảm giá vào phương thức riêng của nó và đặt cho nó một tên có ý nghĩa:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Giá cuối: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**Đầu ra:**
```
Giá cuối: 80.0
```

Bằng cách trích xuất tính toán vào một phương thức, bạn bây giờ có một hoạt động được định nghĩa rõ ràng có thể được tái sử dụng, kiểm thử độc lập, và dễ dàng chỉnh sửa.

### Ví dụ 2: Làm Giản đơn Biểu thức Điều kiện

Trước khi tái cấu trúc, các câu lệnh điều kiện có thể quá phức tạp hoặc khó đọc:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Giảm giá: $discount");
}
```

**Đầu ra:**
```
Giảm giá: 0.05
```

Sau khi tái cấu trúc, hãy cân nhắc sử dụng map để có cấu trúc rõ ràng hơn và cập nhật hoặc mở rộng loại khách hàng và khoản giảm giá dễ dàng hơn:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Giảm giá: $discount");
}
```

**Đầu ra:**
```
Giảm giá: 0.05
```

Việc tái cấu trúc này không chỉ làm cho code ngắn gọn hơn mà còn kết nạp logic xác định giảm giá theo một cách dễ hiểu và dễ bảo trì hơn.

### Thư viện của Bên Thứ Ba cho Tái Cấu trúc

Khi nói đến tái cấu trúc trong Dart, đặc biệt là trong các ứng dụng Flutter, bộ công cụ [Dart DevTools](https://dart.dev/tools/dart-devtools) là vô giá. Nó bao gồm các công cụ hiệu suất, trình kiểm tra widget, và debugger ở cấp mã nguồn. Mặc dù không phải là thư viện bên thứ ba, Dart DevTools thường được sử dụng cùng với các thư viện như `flutter_bloc` để quản lý trạng thái một cách sạch sẽ, thúc đẩy việc tái cấu trúc để cải thiện tính mô-đun và khả năng đọc. Thật không may, do phạm vi của bài viết này, các ví dụ mã sử dụng thư viện của bên thứ ba không được cung cấp ở đây, nhưng các nhà phát triển được khuyến khích khám phá các công cụ này để nâng cao quá trình tái cấu trúc trong các ứng dụng Dart/Flutter của mình.
