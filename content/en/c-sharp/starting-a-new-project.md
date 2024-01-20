---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project in C# means setting up a structured code environment for developing, testing, and maintaining a particular software solution. Programmers do this to organize code, track changes and collaborate efficiently.

## How to:

Here's how to create a new console app in C#: 

```C#
// Call up the dotnet CLI
$ dotnet new console -n HelloWorld
// This creates a new console application named HelloWorld
```
  
Navigate into the new project folder `HelloWorld` with the command `cd HelloWorld` and launch the project by typing:

```C#
// Call up the dotnet CLI
$ dotnet run
```

You'll see an output like:

```C#
Hello, World!
```

This is a basic structure of a C# console application project. You'll find two files: `Program.cs` (the entry point for the application), and `.csproj` which holds metadata and instructions.

## Deep Dive

Historically, C# projects were created in Visual Studio which provided scaffolding via GUI; with the advent of .NET Core, cross-platform and command-line-interface was made possible for creating and managing projects.

Project templates, an alternative to manually creating files, offer common application types (Console, Web API, etc.), and provide all necessary boilerplates, saving time and effort.

The .NET CLI command `dotnet new` creates a new project from a template. The `-n` (or `--name`) option specifies the name of the new project. The project file (`.csproj`) holds instructions for building the project, including target framework info and package references.

## See Also

To explore the wide range of C# project types and templates, check out the official .NET documentation: [Project templates for .NET CLI](https://docs.microsoft.com/dotnet/core/tutorials/cli-templates-create-projects-with-dotnet-new)

For more in-depth information about .NET project structure and files, see: [.NET project structure overview](https://docs.microsoft.com/dotnet/core/tools/dotnet-new)