/*-- CUSTOM HELPER FUNCTIONS --*/
// This is a good place to define reusable functions.

// A function to draw a styled box around content.
// It will be automatically called by Quarto for any ::: {.boxed-text} div.
#let boxed-text(body) = {
  rect(
    inset: 10pt,
    radius: 4pt,
    stroke: black,
    body
  )
}

#let PrettyTypst(
  // The document title.
  title: "PrettyTypst",
  subtitle: none,
  // Logo in top right corner.
  typst-logo: none,

  // The document content.
  body
) = {

  // Set document metadata.
  set document(title: title)
  
  // Configure pages.
  set page(
    margin: (left: 1in, right: 1in, top: 1in, bottom: 1in),
    numbering: "1",
    number-align: right,
    //background: place(right + top, rect(
    //  fill: rgb("#e2583eff"),
    //  height: 100%,
    //  width: 1cm,
    // ))
  )
  
  // Configure table style
  
  set table(
    stroke: 0pt,
    fill: white,
    inset: 10pt,
  )

  // Set the body font.
  set text(12pt, font: "Palatino")

  // Configure headings.
  
  show heading.where(level: 1): it =>{
    set block(above: 0.8em, below: 0.8em)
    set align(center)
    set text(14pt, weight: "bold")
    upper(it.body)
  }
  show heading.where(level: 2): it => {
    set block(above: 0.5cm, below: 0.5cm)
    set align(left)
    set text(12pt, style: "italic", weight: "regular")
    upper(it.body)
    }


  // Tigerlily border column
  grid(
    columns: (1fr, 0.75cm),
  //  column-gutter: 1cm,

    // HEADER
    stack(
      dir: ttb,
      spacing: 4em, // Space between logo and title block

      // Logo (only appears if provided in the YAML)
      if typst-logo != none {
        align(center)[
          #image(typst-logo.path, width: 30%)
        ]
      },

      // Title and Subtitle block
      pad(bottom: 1cm, {
        stack(
          dir: ttb,      // Arrange items top-to-bottom
          spacing: 2em, // Add a little space between them

          // Main Title (styled as before)
          align(center, text(font: "Palatino", 20pt, weight: 800, upper(title))),

          // Subtitle (only appears if it's provided in the YAML)
          if subtitle != none {
            align(center, text(font: "Palatino", 14pt, weight: 400, subtitle))
          }
        )
      })
    ),

    // Empty block to act as a placeholder for the top-right grid cell.
    { },
    
    // The main body text.
    {
      set par(justify: true)
      body
      v(1fr)
    },
  )
}


