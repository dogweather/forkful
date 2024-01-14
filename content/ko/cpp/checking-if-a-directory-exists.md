---
title:    "C++: 디렉토리가 존재하는지 확인하기"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜
프로그래밍을 하다 보면 특정한 디렉토리가 존재하는지 확인하는 일이 필요할 때가 있습니다. 이를 할 수 있는 방법에 대해서 알아보겠습니다.

## 어떻게
디렉토리의 존재 여부를 알려면 "filesystem" 라이브러리를 사용해야 합니다. 먼저 다음과 같이 헤더 파일을 추가합니다.

```C++
#include <filesystem>
```

그리고 다음과 같은 코드를 사용하면 해당 디렉토리가 존재하는지 여부를 알 수 있습니다.

```C++
std::filesystem::path myPath("myDirectory"); 
if (std::filesystem::exists(myPath)) { 
    std::cout << "This directory exists!" << std::endl; 
} else { 
    std::cout << "This directory does not exist." << std::endl; 
}
```

존재하는 디렉토리를 myPath에 할당하고 exists() 메소드를 호출해서 디렉토리가 존재하는지 체크합니다. 만약 디렉토리가 존재한다면 "This directory exists!" 메시지가 출력되고, 존재하지 않는다면 "This directory does not exist." 메시지가 출력됩니다.

## Deep Dive
디렉토리의 존재 여부를 체크하는 기능은 파일 시스템을 다룰 때 필수적입니다. 디렉토리가 없다면 파일에 접근할 수 없기 때문입니다. C++ 17부터는 "filesystem" 라이브러리를 지원하므로 간단하게 디렉토리의 존재 여부를 체크할 수 있게 되었습니다.

또한 exists() 메소드 대신 연산자 오버로딩을 이용해서도 디렉토리의 존재 여부를 체크할 수 있습니다. 다음과 같은 코드를 사용하면 동일한 결과를 얻을 수 있습니다.

```C++
std::filesystem::path myPath("myDirectory"); 
if (myPath) { 
    std::cout << "This directory exists!" << std::endl; 
} else { 
    std::cout << "This directory does not exist." << std::endl; 
}
```

## See Also
- [C++ "filesystem" 라이브러리 문서](https://ko.cppreference.com/w/cpp/filesystem)
- [C++ 17 파일 시스템 작업](https://cpplover.blogspot.com/2016/04/c17.html)
- [디렉토리 및 파일 존재 유무 확인하기](https://aran.tistory.com/10)