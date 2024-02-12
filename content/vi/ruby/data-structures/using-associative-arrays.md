---
title:                "Sử dụng mảng liên kết"
aliases:
- /vi/ruby/using-associative-arrays.md
date:                  2024-01-30T19:13:22.578091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Mảng kết hợp, hay còn được biết đến nhiều hơn với tên gọi là bảng băm trong Ruby, cho phép ghép cặp khóa duy nhất với giá trị. Chúng là thứ không thể thiếu khi bạn cần theo dõi các phần tử thông qua một tham chiếu cụ thể, như lưu trữ các thuộc tính của một đối tượng hoặc truy cập nhanh dữ liệu qua một định danh duy nhất.

## Làm thế nào:

Tạo và sử dụng bảng băm trong Ruby rất đơn giản. Bạn có thể khởi tạo một bảng băm trống, điền nó với các cặp khóa-giá trị, truy cập giá trị qua các khóa của chúng, và nhiều hơn nữa. Dưới đây là cách bạn thực hiện:

```Ruby
# Tạo một bảng băm
my_hash = { "name" => "John Doe", "age" => 30 }

# Một cách khác để tạo bảng băm
another_hash = Hash.new
another_hash["position"] = "Nhà phát triển"

# Truy cập giá trị bảng băm
puts my_hash["name"] # Đầu ra: John Doe

# Thêm một cặp khóa-giá trị mới
my_hash["language"] = "Ruby"
puts my_hash # Đầu ra: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# Lặp qua bảng băm
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# Đầu ra:
# name: John Doe
# age: 30
# language: Ruby
```

Bạn cũng có thể sử dụng biểu tượng như khóa hiệu quả hơn:

```Ruby
# Sử dụng biểu tượng cho khóa
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # Đầu ra: Jane Doe
```

## Sâu hơn nữa:

Khái niệm về mảng kết hợp không phải là duy nhất của Ruby; nhiều ngôn ngữ thực hiện chúng dưới các tên gọi khác nhau, như từ điển trong Python hay đối tượng trong JavaScript (khi được sử dụng như cặp khóa-giá trị). Trong giai đoạn đầu của Ruby, bảng băm hơi chậm và không linh hoạt lắm. Tuy nhiên, theo thời gian, cách thực hiện bảng băm của Ruby đã trở nên rất tối ưu, đặc biệt là cho khóa biểu tượng, làm cho chúng trở nên cực kỳ hiệu quả cho việc truy cập và cập nhật thường xuyên.

Bảng băm của Ruby nổi bật với sự dễ sử dụng về cú pháp và tính linh hoạt - bạn có thể sử dụng gần như bất kỳ loại đối tượng nào làm khóa, mặc dù biểu tượng và chuỗi là phổ biến nhất. Internally, bảng băm của Ruby được thực hiện bằng cách sử dụng thuật toán băm cân bằng giữa tốc độ và hiệu quả bộ nhớ, ngay cả khi số lượng phần tử tăng lên.

Mặc dù bảng băm rất linh hoạt, chúng không phải là giải pháp toàn diện cho việc lưu trữ dữ liệu trong Ruby. Đối với các bộ sưu tập có thứ tự, mảng là phù hợp hơn, và cho các tập hợp các mục duy nhất, một Set có thể là lựa chọn tốt hơn. Ngoài ra, đối với cấu trúc dữ liệu rất phức tạp, việc tạo ra các lớp tùy chỉnh có thể được khuyến khích.

Nhớ rằng, việc lựa chọn sử dụng bảng băm so với các cấu trúc dữ liệu khác chủ yếu dựa trên trường hợp sử dụng cụ thể—bảng băm xuất sắc trong việc tìm kiếm nhanh và duy trì quan hệ giữa khóa duy nhất và giá trị của chúng.
