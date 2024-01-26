---
title:                "Starting a new project"
date:                  2024-01-20T18:02:56.464432-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new C# project means setting up a fresh solution and project files that structure your code. Programmers kick off new projects to turn ideas into software, solve problems, or explore tech.

## How to:
Let's roll up our sleeves and get our hands on some code. Assume you've got .NET 6 or later - that's the latest at the time of writing. You'll use the .NET CLI for this.

Create a new console app:
```C#
dotnet new console -o MyNewProject
```
Hop into your project directory:
```C#
cd MyNewProject
```
Run your fresh, boilerplate Hello World:
```C#
dotnet run
```
You should see:
```
Hello, World!
```
Your new project is off the ground!

## Deep Dive
Back in the day, you'd probably fire up Visual Studio and click through a wizard. Not anymore - now the .NET CLI is the go-to. It's quick and doesn't assume much about your dev environment.

Alternatives? You bet. Visual Studio is still there for a GUI experience. Rider and Visual Studio Code are solid picks too. But the CLI? It's all about that lean, mean scripting vibe.

Implementation details? Your `.csproj` file holds the keys to the kingdom. It's XML, but don't sweat - it pretty much takes care of itself. Here lies info your build process needs - target framework, dependencies, project references, all the good stuff.

## See Also
- [Official .NET CLI Documentation](https://docs.microsoft.com/en-us/dotnet/core/tools/)
- [Visual Studio Product Page](https://visualstudio.microsoft.com/)
- [.NET Project SDK Overview](https://docs.microsoft.com/en-us/dotnet/core/project-sdk/overview)
