---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Lua: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

디렉토리가 존재하는지 확인하는 것은 주어진 경로에 특정 디렉토리가 있는지 여부를 확인하는 것입니다. 프로그래머들은 이 작업을 하는 이유는 사용자가 제대로 된 경로를 입력했는지를 확인하고, 프로그램의 안정성을 위해 오류 처리를 할 수 있도록 하기 위함입니다.

## 하는 방법:

```Lua
if lfs.attributes("path_to_directory", "mode") == "directory" then
    print("The directory exists!")
else
    print("The directory does not exist!")
end
```

위 예제에서, ```lfs.attributes()``` 함수를 사용하여 주어진 경로의 속성을 확인합니다. 디렉토리가 존재하는지 확인하기 위해, mode 매개변수에 "directory" 값을 전달하여 디렉토리인지 아닌지를 확인합니다. ```lfs```는 Lua 패키지인 LuaFileSystem의 약자로, 디렉토리 및 파일을 다루는 함수들을 제공합니다.

## 깊게 들어가기:

디렉토리가 존재하는지 확인하는 기능은 운영체제와 관련이 있습니다. 이 기능은 운영체제의 파일 시스템에 직접 접근하여 경로의 속성을 확인하기 때문입니다. 따라서, 운영체제마다 다른 결과가 나올 수 있으므로 주의해야 합니다.

다른 방법으로는 파일을 여는 작업을 수행해보는 것이 있습니다. 만약 파일이 성공적으로 열리면 디렉토리가 존재하는 것으로 간주할 수 있습니다. 하지만 이 방법은 불필요한 작업을 수행하게 되어 비효율적일 수 있습니다.

디렉토리가 존재하는지 확인하기 위해서는 운영체제의 시스템 호출을 사용하는 것이 가장 정확한 방법입니다. 하지만 이 방법은 실행 속도가 느리기 때문에 대부분의 프로그래머들은 위에서 언급한 ```lfs``` 함수를 사용하여 구현합니다.

## 관련 자료:

- [LuaFileSystem](https://keplerproject.github.io/luafilesystem/): Lua에서 파일 및 디렉토리를 다루는 패키지입니다.
- [Lua Reference Manual](https://www.lua.org/manual/5.3/): Lua의 공식 문서입니다. 파일 및 디렉토리 관련 함수들의 설명이 있습니다.