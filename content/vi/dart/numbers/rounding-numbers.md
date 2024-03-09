---
title:                "Làm tròn số"
date:                  2024-03-08T21:56:23.836019-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Làm thế nào & Tại sao?

Làm tròn số là quá trình điều chỉnh một số để gần nhất với số nguyên hoặc đến một số lượng chữ số thập phân được chỉ định. Lập trình viên thường làm tròn số để đơn giản hóa các tính toán, cải thiện khả năng đọc hoặc chuẩn bị dữ liệu cho việc hiển thị, đảm bảo tính nhất quán và rõ ràng trong kết quả số học.

## Cách thực hiện:

Dart cung cấp các phương thức bản địa trong kiểu `num` của nó cho các thao tác làm tròn. Tại đây, chúng ta sẽ khám phá các phương thức như `round()`, `floor()`, `ceil()`, và cách làm tròn đến một số lượng chữ số thập phân nhất định.

### Làm tròn đến số nguyên gần nhất:

```dart
var number = 3.56;
print(number.round()); // Đầu ra: 4
```

### Làm tròn xuống:

```dart
print(number.floor()); // Đầu ra: 3
```

### Làm tròn lên:

```dart
print(number.ceil()); // Đầu ra: 4
```

### Làm tròn đến một số lượng chữ số thập phân nhất định:

Để làm tròn đến một số lượng chữ số thập phân nhất định, chúng ta có thể sử dụng phương thức `toStringAsFixed()`, phương thức này trả về một chuỗi, hoặc sử dụng sự kết hợp của `pow` từ `dart:math` để có kết quả số.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // Dành cho mục đích hiển thị
print(roundedString); // Đầu ra: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Đầu ra: 3.57

// Hoặc, cho một kết quả số:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Đầu ra: 3.57
```

Mặc dù thư viện cốt lõi của Dart đáp ứng hầu hết các nhu cầu làm tròn một cách hiệu quả, đối với những thao tác toán học phức tạp hơn hoặc các yêu cầu làm tròn chính xác, các thư viện như `decimal` có thể hữu ích. Thư viện `decimal` cung cấp một cách dễ dàng để làm việc với số thập phân mà không mất độ chính xác, điều này đặc biệt hữu ích cho các tính toán tài chính, nhưng đối với các phương pháp làm tròn đơn giản như đã cho thấy, chức năng cốt lõi của Dart thường là đủ.
