---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:34.296956-07:00
description: "L\xE0m th\u1EBF n\xE0o: YAML kh\xF4ng \u0111\u01B0\u1EE3c t\xEDch h\u1EE3\
  p s\u1EB5n trong C++. B\u1EA1n s\u1EBD c\u1EA7n m\u1ED9t th\u01B0 vi\u1EC7n nh\u01B0\
  \ `yaml-cpp`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch ph\xE2n t\xEDch m\u1ED9t\
  \ t\u1EC7p YAML \u0111\u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:37.068766-06:00'
model: gpt-4-0125-preview
summary: "YAML kh\xF4ng \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n trong C++."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
