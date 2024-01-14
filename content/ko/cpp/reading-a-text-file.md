---
title:                "C++: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것이 왜 중요할까요? C++ 프로그래머들이 텍스트 파일을 읽는 이유는 많습니다. 첫째, 텍스트 파일은 데이터를 저장하고 전송하는 가장 일반적인 방법 중 하나입니다. 둘째, 텍스트 파일은 우리가 작성한 프로그램에서 입력을 받는 방법입니다. 셋째, 텍스트 파일은 프로그램에서 출력을 디스플레이하는 데 사용될 수 있습니다.

# 어떻게

텍스트 파일을 읽는 방법을 배우려면 C++을 사용하세요. 텍스트 파일을 읽기 위해서는 **ifstream** 객체를 사용해야합니다. 아래는 "example.txt" 파일에서 텍스트를 읽는 예제 코드입니다.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main(){
  ifstream myFile("example.txt"); // open the file
  string line;

  while(getline(myFile, line)){ // read each line of the file
    cout << line << endl; // output the line to the console
  }

  myFile.close(); // close the file
  return 0;
}
```

위의 코드를 실행하면 "example.txt" 파일의 내용이 한 줄씩 콘솔에 출력됩니다.

```
Hello World
This is a sample text file.
```

# 깊이 알아보기

텍스트 파일을 읽는 방법에 대해 더 알아보겠습니다. 첫째, 텍스트 파일을 읽을 때 고려해야하는 것은 파일이 존재하는지 확인하는 것입니다. 또한 파일이 지정된 위치에 있는지도 확인해야합니다. 둘째, 파일을 읽는 것 외에도 파일에 쓰거나 수정하는 것도 마찬가지로 중요합니다. 세째, 파일에 대한 액세스 권한을 확인해야합니다. 파일을 쓰는 권한이 없다면 파일에 쓸 수 없습니다.

# 관련 자료

- https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm
- https://www.geeksforgeeks.org/inputoutput-system-calls-c-create-open-close-read-write/
- https://docs.microsoft.com/en-us/cpp/standard-library/reading-and-writing-to-a-file