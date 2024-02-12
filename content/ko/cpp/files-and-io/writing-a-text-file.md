---
title:                "텍스트 파일 쓰기"
aliases:
- /ko/cpp/writing-a-text-file.md
date:                  2024-02-03T19:27:26.228763-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
C++에서 텍스트 파일에 쓰기는 파일을 생성하거나 열고 거기에 데이터를 작성하는 것을 포함합니다. 이것은 로그, 사용자 생성 콘텐츠 또는 설정값과 같이 데이터를 유지해야 하는 응용 프로그램에 필수적인 작업입니다. 프로그래머들은 프로그램 실행 중에 생성된 데이터를 저장하거나 다른 프로그램이나 사용자가 사용할 데이터를 내보내기 위해서 이 작업을 합니다.

## 어떻게:
C++은 텍스트 파일에 쓰는 몇 가지 방법을 제공하지만, 가장 간단한 방법 중 하나는 파일 쓰기 작업에 사용되는 `ofstream` (출력 파일 스트림) 클래스를 제공하는 `<fstream>` 라이브러리를 사용하는 것입니다.

### `<fstream>` 사용 예시:

```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Hello, world!\n";
        file << "C++에서 파일에 쓰기는 간단합니다.";
        file.close();
    } else {
        std::cerr << "파일을 열기에 실패했습니다\n";
    }
    return 0;
}
```

**'example.txt'의 샘플 출력:**
```
Hello, world!
C++에서 파일에 쓰기는 간단합니다.
```

프로그래머가 더 복잡한 데이터를 다루거나 쓰기 과정에 더 많은 제어가 필요한 경우, Boost Filesystem과 같은 제3자 라이브러리를 이용할 수 있습니다.

### Boost Filesystem 사용 예시:

파일 작업을 위해 Boost를 사용하려면 먼저 Boost 라이브러리를 설치해야 합니다. 다음 예시는 `boost::filesystem`과 `boost::iostreams`를 사용하여 파일을 생성하고 그에 쓰는 것을 보여줍니다.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost는 파일 작업을 쉽게 만듭니다.\n";
    out << "이 줄은 Boost로 작성되었습니다.";
    
    return 0;
}
```

**'boost_example.txt'의 샘플 출력:**
```
Boost는 파일 작업을 쉽게 만듭니다.
이 줄은 Boost로 작성되었습니다.
```

특정 프로젝트의 요구 사항과 파일 I/O 작업에 대해 필요한 제어 또는 유연성의 정도에 따라 원시 C++과 Boost와 같은 제3자 라이브러리 사이에 선택할 수 있습니다.
