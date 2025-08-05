Example of an extension for creating responsive and sustainable images in Markdown, using the haskell library MMark.

Complete article at https://injeniero.com/en/blog/responsive-sustainable-images-markdown-commonmark-haskell-mmark

**Use**
1. Create a directory & cd to the directory
2. Run ‘cabal init’ command in the terminal within the directory
3. Create a markdown file, example.md or any name and add some images
4. Copy the sourcecode to the Main.hs file in the /app folder replacing the one created by cabal init
5. Add the dependencies in the.cabal file
6. Run 'cabal run exes -- yourfile.md'
7. See yourfile.html with the resulting HTML
