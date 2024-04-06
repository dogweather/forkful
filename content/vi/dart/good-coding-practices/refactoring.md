---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:50.571678-07:00
description: "L\xE0m th\u1EBF n\xE0o: Tr\u01B0\u1EDBc khi t\xE1i c\u1EA5u tr\xFAc,\
  \ b\u1EA1n c\xF3 th\u1EC3 c\xF3 m\u1ED9t \u0111o\u1EA1n m\xE3 m\xE0 tr\u1ED9n l\u1EAB\
  n nhi\u1EC1u c\u1EA5p \u0111\u1ED9 tr\u1EEBu t\u01B0\u1EE3ng ho\u1EB7c tr\xE1ch\
  \ nhi\u1EC7m, nh\u01B0 t\xEDnh m\u1ED9t kho\u1EA3n gi\u1EA3m gi\xE1 v\xE0 sau \u0111\
  \xF3\u2026"
lastmod: '2024-04-05T21:53:37.691502-06:00'
model: gpt-4-0125-preview
summary: "Tr\u01B0\u1EDBc khi t\xE1i c\u1EA5u tr\xFAc, b\u1EA1n c\xF3 th\u1EC3 c\xF3\
  \ m\u1ED9t \u0111o\u1EA1n m\xE3 m\xE0 tr\u1ED9n l\u1EABn nhi\u1EC1u c\u1EA5p \u0111\
  \u1ED9 tr\u1EEBu t\u01B0\u1EE3ng ho\u1EB7c tr\xE1ch nhi\u1EC7m, nh\u01B0 t\xEDnh\
  \ m\u1ED9t kho\u1EA3n gi\u1EA3m gi\xE1 v\xE0 sau \u0111\xF3 \xE1p d\u1EE5ng n\xF3\
  ."
title: "T\xE1i c\u1EA5u tr\xFAc code"
weight: 19
---

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
