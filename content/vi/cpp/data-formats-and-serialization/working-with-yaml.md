---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:34.296956-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi YAML \u0111\u1EC1 c\u1EADp \u0111\u1EBFn vi\u1EC7\
  c ph\xE2n t\xEDch v\xE0 t\u1EA1o d\u1EEF li\u1EC7u trong ng\xF4n ng\u1EEF YAML Ain't\
  \ Markup Language th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng n\xF3\u2026"
lastmod: '2024-03-11T00:14:10.369527-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi YAML \u0111\u1EC1 c\u1EADp \u0111\u1EBFn vi\u1EC7\
  c ph\xE2n t\xEDch v\xE0 t\u1EA1o d\u1EEF li\u1EC7u trong ng\xF4n ng\u1EEF YAML Ain't\
  \ Markup Language th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi. L\u1EADp tr\xEC\
  nh vi\xEAn s\u1EED d\u1EE5ng n\xF3\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Làm gì & Tại sao?

Làm việc với YAML đề cập đến việc phân tích và tạo dữ liệu trong ngôn ngữ YAML Ain't Markup Language thân thiện với con người. Lập trình viên sử dụng nó cho các tệp cấu hình, chuỗi hóa dữ liệu và cài đặt ứng dụng do độ dễ đọc và đơn giản của nó.

## Làm thế nào:

YAML không được tích hợp sẵn trong C++. Bạn sẽ cần một thư viện như `yaml-cpp`. Dưới đây là cách phân tích một tệp YAML đơn giản:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("config.yaml");
    YAML::Node config = YAML::Load(file);
    
    std::string username = config["user"]["name"].as<std::string>();
    int age = config["user"]["age"].as<int>();
    
    std::cout << "Tên: " << username << ", Tuổi: " << age << std::endl;
    return 0;
}
```

Giả sử `config.yaml` là:
```
user:
  name: John Doe
  age: 30
```

Kết quả:
```
Tên: John Doe, Tuổi: 30
```

## Tìm hiểu sâu hơn

YAML được giới thiệu lần đầu tiên vào năm 2001 như một tiêu chuẩn chuỗi hóa dữ liệu dễ đọc cho con người. Trong khi JSON và XML là các lựa chọn phổ biến khác, cú pháp tối thiểu của YAML đã làm cho nó trở nên phổ biến cho các tệp cấu hình. Các thư viện như `yaml-cpp` xử lý việc phân tích và phát sinh dữ liệu YAML, biểu diễn nó trong các cấu trúc như bản đồ và chuỗi, tương tự như đối tượng và mảng JSON.

## Xem thêm

- Đặc tả YAML 1.2: https://yaml.org/spec/1.2/spec.html
- Kho GitHub yaml-cpp: https://github.com/jbeder/yaml-cpp
- Giới thiệu về YAML: https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started
