---
title:                "Làm việc với CSV"
date:                  2024-03-08T21:57:23.389774-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Làm việc với các tệp CSV (Comma Separated Values - Giá trị Tách bởi Dấu phẩy) bao gồm việc phân tích và tạo các tệp văn bản mà mỗi dòng chứa các giá trị được tách bởi dấu phẩy. Các lập trình viên thực hiện điều này để kích hoạt trao đổi dữ liệu giữa các ứng dụng khác nhau hoặc để tạo điều kiện lưu trữ dữ liệu một cách nhẹ nhàng, dễ đọc.

## Làm thế nào:

Để xử lý tệp CSV trong Dart, bạn thường xử lý văn bản thủ công hoặc sử dụng các thư viện bên thứ ba để đơn giản hóa công việc. Ở đây, chúng ta sẽ xem xét cả hai cách tiếp cận.

### Phân tích CSV Thủ công

Nếu nhu cầu của bạn đơn giản, bạn có thể chọn phân tích cú pháp một chuỗi CSV một cách thủ công. Điều này có thể được thực hiện bằng cách sử dụng các hàm thao tác chuỗi cốt lõi của Dart:

```dart
void main() {
  // Dữ liệu CSV mẫu
  String csvData = "Tên,Tuổi,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Chia dữ liệu CSV thành các dòng
  List<String> lines = csvData.split('\n');
  
  // Phân tích từng dòng
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // Xuất dữ liệu đã phân tích
  print(data);
}

// Dữ liệu đầu ra mẫu:
// [{Tên: John Doe, Tuổi: 30, Email: john@example.com}, {Tên: Jane Smith, Tuổi: 25, Email: jane@example.com}]
```

### Sử dụng Thư viện Bên thứ Ba: `csv`

Đối với các tình huống phức tạp hơn hoặc để đơn giản hóa mã của bạn, bạn có thể sử dụng một thư viện bên thứ ba phổ biến như `csv`. Đầu tiên, hãy thêm nó vào dự án của bạn bằng cách bao gồm `csv: ^5.0.0` (hoặc phiên bản mới nhất) trong tệp `pubspec.yaml` của bạn dưới `dependencies`. Sau đó sử dụng nó như sau:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Tên,Tuổi,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Sử dụng CsvToListConverter để phân tích dữ liệu CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // Mục danh sách đầu tiên chứa tiêu đề
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Xóa hàng tiêu đề trước khi xử lý tiếp
  listData.removeAt(0);
  
  // Chuyển đổi thành List<Map<String, dynamic>> để có định dạng cấu trúc tốt hơn
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Xuất dữ liệu đã ánh xạ
  print(mappedData);
}

// Dữ liệu đầu ra mẫu:
// [{Tên: John Doe, Tuổi: 30, Email: john@example.com}, {Tên: Jane Smith, Tuổi: 25, Email: jane@example.com}]
```

Cả hai phương pháp đều minh họa cách làm việc với dữ liệu CSV: phương pháp đầu tiên là thủ công, dành cho mục đích học tập hoặc khi xử lý các cấu trúc CSV rất đơn giản; phương pháp thứ hai, bằng cách tận dụng một thư viện mạnh mẽ giúp đơn giản hóa việc phân tích và có thể xử lý các độ phức tạp khác nhau của định dạng CSV.
