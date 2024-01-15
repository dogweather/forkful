---
title:                "폴더의 존재 여부 확인하기"
html_title:           "C++: 폴더의 존재 여부 확인하기"
simple_title:         "폴더의 존재 여부 확인하기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍에서 디렉토리가 존재하는지 확인하는 것은 중요한 작업입니다. 이를 통해 파일 시스템의 상태를 파악하고, 원하는 작업을 수행할 수 있습니다. 따라서, 디렉토리 존재 확인은 프로그래머에게 매우 유용한 기능입니다.

## 방법

```C++
#include <iostream>
#include <filesystem>

int main(){
	// 존재하는 디렉토리 경로를 지정합니다.
	std::filesystem::path dirPath = "/Users/username/Desktop";
	
	// 디렉토리가 존재하는지 확인합니다.
	if(std::filesystem::exists(dirPath)){
		std::cout << "디렉토리가 존재합니다." << std::endl;
	}
	else{
		std::cout << "디렉토리가 존재하지 않습니다." << std::endl;
	}
	
	return 0;
}
```

위 코드는 `<filesystem>` 라이브러리를 이용하여 디렉토리가 존재하는지 확인하는 예시입니다. 사용되는 함수는 `exists()` 이며, 인자로 디렉토리의 경로를 받아 불리언 값으로 디렉토리의 존재 여부를 반환합니다.

아래는 예시 출력 결과입니다.

```
디렉토리가 존재합니다.
```

## 딥 다이브

디렉토리 존재 확인은 파일 시스템 작업에 있어서 빈번하게 사용되며, 검사해야 할 디렉토리의 깊이나 수가 많을수록 더 유용한 기능이 됩니다. 또한, 심볼릭 링크와 같은 다른 파일 시스템의 구성 요소들도 포함하여 디렉토리가 실제로 존재하는지를 확인할 수 있습니다.

## 관련 항목

- [std::filesystem::exists()](https://en.cppreference.com/w/cpp/filesystem/exists)
- [C++17의 파일 시스템 라이브러리](https://docs.microsoft.com/ko-kr/cpp/standard-library/filesystem)
- [파일 시스템 작업에 유용한 도구들](https://www.tutorialspoint.com/cplusplus/cpp_file_io.htm)