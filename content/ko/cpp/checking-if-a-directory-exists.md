---
title:                "디렉토리의 존재 여부 확인"
html_title:           "C++: 디렉토리의 존재 여부 확인"
simple_title:         "디렉토리의 존재 여부 확인"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디렉토리가 존재하는지 확인하는 것이 무엇인지 알아볼까요? 프로그래머들이 이 작업을 수행하는 이유는 무엇일까요? 디렉토리 존재 확인은 주어진 경로가 실제로 디렉토리인지 아닌지를 확인하기 위한 중요한 작업입니다. 프로그래머들은 이를 통해 파일을 업로드하거나 지정된 디렉토리에 파일을 만들 수 있는지 확인할 수 있습니다.

## 어떻게:
디렉토리가 존재하는지 확인하기 위해서는 여러 가지 방법이 있습니다. 가장 간단한 방법은 ```opendir()``` 함수를 사용하는 것입니다. 이 함수는 주어진 경로에 디렉토리가 있는지 확인하고 존재하는 경우 해당 디렉토리의 주소를 반환합니다. 다음은 이 함수를 사용한 예시 코드입니다.

```C++
DIR* directory = opendir("경로");
if (directory) {
    cout << "디렉토리가 존재합니다." << endl;
    closedir(directory);
} else {
    cout << "디렉토리가 존재하지 않습니다." << endl;
}
```

위 코드는 지정된 경로에 디렉토리가 있는 경우 "디렉토리가 존재합니다."를 출력하고, 그렇지 않은 경우 "디렉토리가 존재하지 않습니다."를 출력합니다.

## 깊이 파고들기:
디렉토리가 존재하는지 확인하는 작업은 이전부터 많이 사용되어 온 방법입니다. 예전에는 ```stat()``` 함수를 사용하여 파일의 정보를 가져와서 해당 파일의 속성 중 하나로 디렉토리 여부를 판단했습니다. 하지만 ```opendir()``` 함수를 사용하면 더 간단하고 효율적인 방법으로 디렉토리 존재 여부를 확인할 수 있습니다.

다른 대안으로는 C++의 ```filesystem``` 라이브러리를 사용하는 것입니다. 이 라이브러리는 파일 정보를 쉽게 가져오고 수정할 수 있도록 다양한 함수와 클래스를 제공합니다. 하지만 이 라이브러리는 C++17부터 추가된 기능이므로 이전 버전에서는 사용할 수 없는 것에 유의해야 합니다.

## 관련 자료:
- [디렉토리 존재 확인하기 (Cplusplus.com)](https://www.cplusplus.com/forum/windows/254801/)
- [C++ 디렉토리 존재 확인하기 (GeeksforGeeks)](https://www.geeksforgeeks.org/program-check-file-directory-exists-not/)